/**
 * cgen.cc
 *
 * Cool MIPS Code Generator
 * Standardized Formatting Version
 */

#include "cgen.h"
#include "cgen_gc.h"
#include <string>

using std::string;

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

// ---------------------------------------------------------
// 辅助函数实现 (Auxiliary Functions)
// ---------------------------------------------------------

Boolean copy_Boolean(Boolean b) {
    return b;
}

void assert_Boolean(Boolean) {}

void dump_Boolean(ostream& stream, int padding, Boolean b) {
    stream << pad(padding) << (int)b << "\n";
}

// ---------------------------------------------------------
// 全局符号定义 (Global Symbols)
// ---------------------------------------------------------

Symbol arg, arg2, Bool, concat, cool_abort, copy, Int, in_int, in_string, IO, length, Main, main_meth, No_class, No_type, Object, out_int, out_string, prim_slot, self, SELF_TYPE, Str, str_field, substr, type_name, val;

static void init_common_symbols(void) {
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

static char *gc_init_names[] = { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] = { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };

BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);
CgenClassTable *g_class_table_ptr = NULL;

// ---------------------------------------------------------
// CgenEnvironment 实现
// ---------------------------------------------------------

int CgenEnvironment::find_attrib_offset(Symbol sym) {
    if (m_current_class_node) {
        std::map<Symbol, int>& attrib_map = m_current_class_node->get_attrib_idx_map();
        if (attrib_map.find(sym) != attrib_map.end()) {
            return attrib_map[sym];
        }
    }
    return -1;
}

// ---------------------------------------------------------
// 汇编发射辅助函数 (Emit Helpers)
// ---------------------------------------------------------

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s) {
    s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s) {
    s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")" << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s) {
    s << LI << dest_reg << " " << val << endl;
}

static void emit_load_address(char *dest_reg, char *address, ostream& s) {
    s << LA << dest_reg << " " << address << endl;
}

static void emit_partial_load_address(char *dest_reg, ostream& s) {
    s << LA << dest_reg << " ";
}

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s) {
    emit_partial_load_address(dest, s);
    b.code_ref(s);
    s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s) {
    emit_partial_load_address(dest, s);
    str->code_ref(s);
    s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s) {
    emit_partial_load_address(dest, s);
    i->code_ref(s);
    s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s) {
    s << MOVE << dest_reg << " " << source_reg << endl;
}

static void emit_neg(char *dest, char *src1, ostream& s) {
    s << NEG << dest << " " << src1 << endl;
}

static void emit_add(char *dest, char *src1, char *src2, ostream& s) {
    s << ADD << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addu(char *dest, char *src1, char *src2, ostream& s) {
    s << ADDU << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addiu(char *dest, char *src1, int imm, ostream& s) {
    s << ADDIU << dest << " " << src1 << " " << imm << endl;
}

static void emit_div(char *dest, char *src1, char *src2, ostream& s) {
    s << DIV << dest << " " << src1 << " " << src2 << endl;
}

static void emit_mul(char *dest, char *src1, char *src2, ostream& s) {
    s << MUL << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sub(char *dest, char *src1, char *src2, ostream& s) {
    s << SUB << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sll(char *dest, char *src1, int num, ostream& s) {
    s << SLL << dest << " " << src1 << " " << num << endl;
}

static void emit_jalr(char *dest, ostream& s) {
    s << JALR << "\t" << dest << endl;
}

static void emit_jal(char *address, ostream& s) {
    s << JAL << address << endl;
}

static void emit_return(ostream& s) {
    s << RET << endl;
}

static void emit_gc_assign(ostream& s) {
    s << JAL << "_GenGC_Assign" << endl;
}

static void emit_disptable_ref(Symbol sym, ostream& s) {
    s << sym << DISPTAB_SUFFIX;
}

void emit_init_ref(Symbol sym, ostream& s) {
    s << sym << CLASSINIT_SUFFIX;
}

static void emit_label_ref(int l, ostream& s) {
    s << "label" << l;
}

static void emit_protobj_ref(Symbol sym, ostream& s) {
    s << sym << PROTOBJ_SUFFIX;
}

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s) {
    s << classname << METHOD_SEP << methodname;
}

static void emit_label_def(int l, ostream& s) {
    emit_label_ref(l, s);
    s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream& s) {
    s << BEQZ << source << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream& s) {
    s << BEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream& s) {
    s << BNE << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream& s) {
    s << BLEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream& s) {
    s << BLT << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream& s) {
    s << BLT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream& s) {
    s << BGT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_branch(int l, ostream& s) {
    s << BRANCH;
    emit_label_ref(l, s);
    s << endl;
}

static void emit_push_stack(char *reg, ostream& str) {
    emit_store(reg, 0, SP, str);
    emit_addiu(SP, SP, -4, str);
}

static void emit_fetch_int_attr(char *dest, char *source_obj, ostream& s) {
    emit_load(dest, DEFAULT_OBJFIELDS, source_obj, s);
}

static void emit_store_int_attr(char *source_val, char *dest_obj, ostream& s) {
    emit_store(source_val, DEFAULT_OBJFIELDS, dest_obj, s);
}

static void emit_test_collector(ostream& s) {
    emit_push_stack(ACC, s);
    emit_move(ACC, SP, s);
    emit_move(A1, ZERO, s);
    s << JAL << gc_collect_names[cgen_Memmgr] << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(ACC, 0, SP, s);
}

static void emit_gc_check(char *source, ostream& s) {
    if (source != (char*)A1) emit_move(A1, source, s);
    s << JAL << "_gc_check" << endl;
}

// ---------------------------------------------------------
// 常量表项方法 (Entry Methods)
// ---------------------------------------------------------

void StringEntry::code_ref(ostream& s) {
    s << STRCONST_PREFIX << index;
}

void StringEntry::code_def(ostream& s, int stringclasstag) {
    IntEntryP lensym = inttable.add_int(len);
    s << WORD << "-1" << endl;
    code_ref(s);
    s << LABEL << WORD << stringclasstag << endl 
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len + 4) / 4) << endl 
      << WORD;
    s << Str << DISPTAB_SUFFIX << endl;
    s << WORD; lensym->code_ref(s); s << endl;
    emit_string_constant(s, str);
    s << ALIGN;
}

void StrTable::code_string_table(ostream& s, int stringclasstag) {
    for (List<StringEntry> *l = tbl; l; l = l->tl()) {
        l->hd()->code_def(s, stringclasstag);
    }
}

void IntEntry::code_ref(ostream& s) {
    s << INTCONST_PREFIX << index;
}

void IntEntry::code_def(ostream& s, int intclasstag) {
    s << WORD << "-1" << endl;
    code_ref(s);
    s << LABEL << WORD << intclasstag << endl 
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl 
      << WORD;
    s << Int << DISPTAB_SUFFIX << endl;
    s << WORD << str << endl;
}

void IntTable::code_string_table(ostream& s, int intclasstag) {
    for (List<IntEntry> *l = tbl; l; l = l->tl()) {
        l->hd()->code_def(s, intclasstag);
    }
}

BoolConst::BoolConst(int i) : val(i) {
    assert(i == 0 || i == 1);
}

void BoolConst::code_ref(ostream& s) const {
    s << BOOLCONST_PREFIX << val;
}

void BoolConst::code_def(ostream& s, int boolclasstag) {
    s << WORD << "-1" << endl;
    code_ref(s);
    s << LABEL << WORD << boolclasstag << endl 
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl 
      << WORD;
    s << Bool << DISPTAB_SUFFIX << endl;
    s << WORD << val << endl;
}

// ---------------------------------------------------------
// CgenClassTable 实现
// ---------------------------------------------------------

CgenClassTable::CgenClassTable(Classes classes, ostream& s) 
    : m_nodes(NULL), m_str(s) {
    
    g_class_table_ptr = this;
    m_tag_string = 4;
    m_tag_int = 2;
    m_tag_bool = 3;

    enterscope();
    Symbol basic_filename = stringtable.add_string("<basic class>");

    // 安装基础类
    addid(No_class, new CgenNode(class_(No_class, No_class, nil_Features(), basic_filename), Basic, this));
    addid(SELF_TYPE, new CgenNode(class_(SELF_TYPE, No_class, nil_Features(), basic_filename), Basic, this));
    addid(prim_slot, new CgenNode(class_(prim_slot, No_class, nil_Features(), basic_filename), Basic, this));

    // Object
    addid(Object, new CgenNode(class_(Object, No_class,
        append_Features(append_Features(
            single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
            single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
            single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
        basic_filename), Basic, this));

    // IO
    addid(IO, new CgenNode(class_(IO, Object,
        append_Features(append_Features(append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)), SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)), SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
        basic_filename), Basic, this));

    // Int
    addid(Int, new CgenNode(class_(Int, Object, 
        single_Features(attr(val, prim_slot, no_expr())), 
        basic_filename), Basic, this));

    // Bool
    addid(Bool, new CgenNode(class_(Bool, Object, 
        single_Features(attr(val, prim_slot, no_expr())), 
        basic_filename), Basic, this));

    // String
    addid(Str, new CgenNode(class_(Str, Object,
        append_Features(append_Features(append_Features(append_Features(
            single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat, single_Formals(formal(arg, Str)), Str, no_expr()))),
            single_Features(method(substr, append_Formals(single_Formals(formal(arg, Int)), 
                single_Formals(formal(arg2, Int))), Str, no_expr()))),
        basic_filename), Basic, this));

    // 构建节点链表
    m_nodes = new List<CgenNode>(probe(Str), m_nodes);
    m_nodes = new List<CgenNode>(probe(Bool), m_nodes);
    m_nodes = new List<CgenNode>(probe(Int), m_nodes);
    m_nodes = new List<CgenNode>(probe(IO), m_nodes);
    m_nodes = new List<CgenNode>(probe(Object), m_nodes);

    // 安装用户类
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        CgenNodeP node = new CgenNode(classes->nth(i), NotBasic, this);
        addid(node->get_name(), node);
        m_nodes = new List<CgenNode>(node, m_nodes);
    }

    build_inheritance_tree();
    
    int tag_counter = 0;
    assign_class_tags(root(), tag_counter);
    
    code();
    exitscope();
}

void CgenClassTable::build_inheritance_tree() {
    for (List<CgenNode> *l = m_nodes; l; l = l->tl()) {
        set_parent_child_relations(l->hd());
    }
}

void CgenClassTable::set_parent_child_relations(CgenNodeP node) {
    Symbol parent_sym = node->get_parent();
    CgenNodeP parent_node = probe(parent_sym);
    node->set_parent(parent_node);
    if (parent_node) {
        parent_node->add_child(node);
    }
}

void CgenClassTable::assign_class_tags(CgenNode* node, int& tag) {
    node->m_class_tag = tag++;
    
    if (node->get_name() == Str) m_tag_string = node->m_class_tag;
    else if (node->get_name() == Int) m_tag_int = node->m_class_tag;
    else if (node->get_name() == Bool) m_tag_bool = node->m_class_tag;
    
    m_class_tag_table[node->get_name()] = node->m_class_tag;
    
    List<CgenNode> *children = node->get_children();
    for (List<CgenNode> *l = children; l; l = l->tl()) {
        assign_class_tags(l->hd(), tag);
    }
    node->m_max_child_tag = tag - 1;
}

void CgenClassTable::code() {
    if (cgen_debug) cout << "Coding global data" << endl;
    emit_global_data();

    if (cgen_debug) cout << "Choosing gc" << endl;
    emit_gc_select();

    if (cgen_debug) cout << "Coding constants" << endl;
    emit_constants();

    emit_class_name_tab();
    emit_class_obj_tab();
    emit_dispatch_tables();
    emit_prot_objs();

    if (cgen_debug) cout << "Coding global text" << endl;
    emit_global_text();

    emit_class_inits();
    emit_class_methods();
}

CgenNodeP CgenClassTable::root() {
    return probe(Object);
}

void CgenClassTable::emit_global_data() {
    Symbol main     = idtable.lookup_string(MAINNAME);
    Symbol string   = idtable.lookup_string(STRINGNAME);
    Symbol integer  = idtable.lookup_string(INTNAME);

    m_str << "\t.data\n" << ALIGN;
    m_str << GLOBAL << CLASSNAMETAB << endl;
    m_str << GLOBAL; emit_protobj_ref(main, m_str); m_str << endl;
    m_str << GLOBAL; emit_protobj_ref(integer, m_str); m_str << endl;
    m_str << GLOBAL; emit_protobj_ref(string, m_str); m_str << endl;
    m_str << GLOBAL; falsebool.code_ref(m_str); m_str << endl;
    m_str << GLOBAL; truebool.code_ref(m_str); m_str << endl;
    m_str << GLOBAL << INTTAG << endl << GLOBAL << BOOLTAG << endl << GLOBAL << STRINGTAG << endl;
    m_str << INTTAG << LABEL << WORD << m_tag_int << endl;
    m_str << BOOLTAG << LABEL << WORD << m_tag_bool << endl;
    m_str << STRINGTAG << LABEL << WORD << m_tag_string << endl;
}

void CgenClassTable::emit_gc_select() {
    m_str << GLOBAL << "_MemMgr_INITIALIZER" << endl 
          << "_MemMgr_INITIALIZER:" << endl 
          << WORD << gc_init_names[cgen_Memmgr] << endl;
    m_str << GLOBAL << "_MemMgr_COLLECTOR" << endl 
          << "_MemMgr_COLLECTOR:" << endl 
          << WORD << gc_collect_names[cgen_Memmgr] << endl;
    m_str << GLOBAL << "_MemMgr_TEST" << endl 
          << "_MemMgr_TEST:" << endl 
          << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}

void CgenClassTable::emit_constants() {
    stringtable.add_string("");
    inttable.add_string("0");
    stringtable.code_string_table(m_str, m_tag_string);
    inttable.code_string_table(m_str, m_tag_int);
    falsebool.code_def(m_str, m_tag_bool);
    truebool.code_def(m_str, m_tag_bool);
}

void CgenClassTable::emit_class_name_tab() {
    m_str << CLASSNAMETAB << LABEL;

    if (m_all_nodes.empty()) {
        for (List<CgenNode> *l = m_nodes; l; l = l->tl()) {
            m_all_nodes.push_back(l->hd());
        }
    }

    std::vector<CgenNode*> sorted_nodes(m_all_nodes.size());
    for (size_t i = 0; i < m_all_nodes.size(); ++i) {
        CgenNode* n = m_all_nodes[i];
        if (n->m_class_tag >= 0 && n->m_class_tag < (int)sorted_nodes.size()) {
            sorted_nodes[n->m_class_tag] = n;
        }
    }

    for (size_t i = 0; i < sorted_nodes.size(); ++i) {
        if (sorted_nodes[i]) {
            m_str << WORD;
            stringtable.lookup_string(sorted_nodes[i]->get_name()->get_string())->code_ref(m_str);
            m_str << endl;
        }
    }
}

void CgenClassTable::emit_class_obj_tab() {
    m_str << CLASSOBJTAB << LABEL;

    if (m_all_nodes.empty()) {
        for (List<CgenNode> *l = m_nodes; l; l = l->tl()) {
            m_all_nodes.push_back(l->hd());
        }
    }

    std::map<int, CgenNode*> tag_map;
    for (size_t i = 0; i < m_all_nodes.size(); ++i) {
        tag_map[m_all_nodes[i]->m_class_tag] = m_all_nodes[i];
    }

    for (int i = 0; i < (int)tag_map.size(); ++i) {
        CgenNode* node = tag_map[i];
        m_str << WORD; emit_protobj_ref(node->get_name(), m_str); m_str << endl;
        m_str << WORD; ::emit_init_ref(node->get_name(), m_str); m_str << endl;
    }
}

void CgenClassTable::emit_dispatch_tables() {
    for (size_t i = 0; i < m_all_nodes.size(); ++i) {
        CgenNode* node = m_all_nodes[i];
        emit_disptable_ref(node->get_name(), m_str); m_str << LABEL;
        
        std::vector<method_class*> methods = node->get_full_methods();
        std::map<Symbol, Symbol>& def_map = node->get_dispatch_def_map();
        
        for (size_t j = 0; j < methods.size(); ++j) {
            method_class* m = methods[j];
            m_str << WORD;
            emit_method_ref(def_map[m->name], m->name, m_str);
            m_str << endl;
        }
    }
}

void CgenClassTable::emit_prot_objs() {
    for (size_t i = 0; i < m_all_nodes.size(); ++i) {
        m_all_nodes[i]->emit_prot_obj(m_str);
    }
}

void CgenClassTable::emit_global_text() {
    m_str << GLOBAL << HEAP_START << endl 
          << HEAP_START << LABEL 
          << WORD << 0 << endl 
          << "\t.text" << endl 
          << GLOBAL;
    ::emit_init_ref(idtable.add_string("Main"), m_str); m_str << endl << GLOBAL;
    ::emit_init_ref(idtable.add_string("Int"), m_str); m_str << endl << GLOBAL;
    ::emit_init_ref(idtable.add_string("String"), m_str); m_str << endl << GLOBAL;
    ::emit_init_ref(idtable.add_string("Bool"), m_str); m_str << endl << GLOBAL;
    emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), m_str); m_str << endl;
}

void CgenClassTable::emit_class_inits() {
    for (size_t i = 0; i < m_all_nodes.size(); ++i) {
        m_all_nodes[i]->emit_init_ref(m_str);
    }
}

void CgenClassTable::emit_class_methods() {
    for (size_t i = 0; i < m_all_nodes.size(); ++i) {
        if (!m_all_nodes[i]->is_basic()) {
            m_all_nodes[i]->emit_method_defs(m_str);
        }
    }
}

// ---------------------------------------------------------
// CgenNode 实现
// ---------------------------------------------------------

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct)
    : class__class((const class__class &)*nd), 
      m_parent(NULL), m_children(NULL), m_basic_status(bstatus), 
      m_class_tag(-1), m_max_child_tag(-1) {
    stringtable.add_string(name->get_string());
}

void CgenNode::add_child(CgenNodeP n) {
    m_children = new List<CgenNode>(n, m_children);
}

void CgenNode::set_parent(CgenNodeP p) {
    assert(m_parent == NULL);
    assert(p != NULL);
    m_parent = p;
}

std::vector<CgenNode*> CgenNode::get_inheritance_path() {
    if (m_inheritance_chain.empty()) {
        CgenNode* curr = this;
        while (curr->get_name() != No_class) {
            m_inheritance_chain.push_back(curr);
            curr = curr->get_parentnd();
        }
        std::reverse(m_inheritance_chain.begin(), m_inheritance_chain.end());
    }
    return m_inheritance_chain;
}

std::vector<attr_class*> CgenNode::get_full_attributes() {
    if (m_layout_attribs.empty()) {
        std::vector<CgenNode*> path = get_inheritance_path();
        int offset = 0;
        for (size_t k = 0; k < path.size(); ++k) {
            Features fs = path[k]->features;
            for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
                if (!fs->nth(i)->is_method()) {
                    attr_class* a = (attr_class*)fs->nth(i);
                    m_layout_attribs.push_back(a);
                    m_attrib_idx_map[a->name] = offset++;
                }
            }
        }
    }
    return m_layout_attribs;
}

std::vector<method_class*> CgenNode::get_full_methods() {
    if (m_layout_methods.empty()) {
        std::vector<CgenNode*> path = get_inheritance_path();
        for (size_t k = 0; k < path.size(); ++k) {
            CgenNode* node = path[k];
            Features fs = node->features;
            for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
                if (fs->nth(i)->is_method()) {
                    method_class* m = (method_class*)fs->nth(i);
                    if (m_dispatch_idx_map.find(m->name) == m_dispatch_idx_map.end()) {
                        m_dispatch_idx_map[m->name] = m_layout_methods.size();
                        m_layout_methods.push_back(m);
                        m_dispatch_def_map[m->name] = node->name;
                    } else {
                        int idx = m_dispatch_idx_map[m->name];
                        m_layout_methods[idx] = m;
                        m_dispatch_def_map[m->name] = node->name;
                    }
                }
            }
        }
    }
    return m_layout_methods;
}

void CgenNode::emit_prot_obj(ostream& s) {
    std::vector<attr_class*> attrs = get_full_attributes();
    get_full_methods(); 

    s << WORD << "-1" << endl;
    emit_protobj_ref(name, s); s << LABEL;
    s << WORD << m_class_tag << "\t# class tag" << endl;
    s << WORD << (DEFAULT_OBJFIELDS + attrs.size()) << "\t# object size" << endl;
    s << WORD; emit_disptable_ref(name, s); s << endl;

    for (size_t i = 0; i < attrs.size(); ++i) {
        attr_class* a = attrs[i];
        s << WORD;
        if (a->type_decl == Int) inttable.lookup_string("0")->code_ref(s);
        else if (a->type_decl == Str) stringtable.lookup_string("")->code_ref(s);
        else if (a->type_decl == Bool) falsebool.code_ref(s);
        else s << "0";
        s << endl;
    }
}

void CgenNode::emit_init_ref(ostream& s) {
    ::emit_init_ref(name, s); s << LABEL;
    
    // Prologue
    emit_addiu(SP, SP, -12, s);
    emit_store(FP, 3, SP, s);
    emit_store(SELF, 2, SP, s);
    emit_store(RA, 1, SP, s);
    emit_addiu(FP, SP, 4, s);
    emit_move(SELF, ACC, s);

    if (m_parent && name != Object) {
        s << JAL; ::emit_init_ref(m_parent->name, s); s << endl;
    }

    Features fs = features;
    for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
        if (!fs->nth(i)->is_method()) {
            attr_class* a = (attr_class*)fs->nth(i);
            if (a->init->get_type() != NULL) {
                CgenEnvironment env;
                env.m_current_class_node = this;
                a->init->code(s, env);
                
                int idx = m_attrib_idx_map[a->name];
                emit_store(ACC, idx + DEFAULT_OBJFIELDS, SELF, s);
                
                if (cgen_Memmgr == 1) {
                    emit_addiu(A1, SELF, 4 * (idx + DEFAULT_OBJFIELDS), s);
                    emit_gc_assign(s);
                }
            }
        }
    }

    // Epilogue
    emit_move(ACC, SELF, s);
    emit_load(FP, 3, SP, s);
    emit_load(SELF, 2, SP, s);
    emit_load(RA, 1, SP, s);
    emit_addiu(SP, SP, 12, s);
    emit_return(s);
}

void CgenNode::emit_method_defs(ostream& s) {
    Features fs = features;
    for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
        if (fs->nth(i)->is_method()) {
            method_class* m = (method_class*)fs->nth(i);
            
            emit_method_ref(name, m->name, s); s << LABEL;
            
            // Prologue
            emit_addiu(SP, SP, -12, s);
            emit_store(FP, 3, SP, s);
            emit_store(SELF, 2, SP, s);
            emit_store(RA, 1, SP, s);
            emit_addiu(FP, SP, 4, s);
            emit_move(SELF, ACC, s);
            
            CgenEnvironment env;
            env.m_current_class_node = this;
            
            int arg_count = m->formals->len();
            for (int j = 0; j < arg_count; ++j) {
                formal_class* f = (formal_class*)m->formals->nth(j);
                env.add_param(f->name, arg_count - 1 - j);
            }
            
            m->expr->code(s, env);
            
            // Epilogue
            emit_load(FP, 3, SP, s);
            emit_load(SELF, 2, SP, s);
            emit_load(RA, 1, SP, s);
            emit_addiu(SP, SP, 12, s);
            emit_addiu(SP, SP, arg_count * 4, s);
            emit_return(s);
        }
    }
}

void program_class::cgen(ostream& os) {
    init_common_symbols();
    g_class_table_ptr = new CgenClassTable(classes, os);
}

// ---------------------------------------------------------
// 表达式代码生成 (Expression Generation)
// ---------------------------------------------------------

static int s_label_counter = 0;
static int next_label() { return s_label_counter++; }

void assign_class::code(ostream& s, CgenEnvironment env) {
    expr->code(s, env);
    int idx = -1;
    
    if ((idx = env.find_var_offset(name)) != -1) {
        int sp_offset = env.next_temp_offset() - idx;
        emit_store(ACC, sp_offset, SP, s);
        if (cgen_Memmgr == 1) {
            emit_addiu(A1, SP, 4 * sp_offset, s);
            emit_gc_assign(s);
        }
    } else if ((idx = env.find_param_offset(name)) != -1) {
        emit_store(ACC, idx + 3, FP, s);
        if (cgen_Memmgr == 1) {
            emit_addiu(A1, FP, 4 * (idx + 3), s);
            emit_gc_assign(s);
        }
    } else if ((idx = env.find_attrib_offset(name)) != -1) {
        emit_store(ACC, idx + DEFAULT_OBJFIELDS, SELF, s);
        if (cgen_Memmgr == 1) {
            emit_addiu(A1, SELF, 4 * (idx + DEFAULT_OBJFIELDS), s);
            emit_gc_assign(s);
        }
    }
}

void static_dispatch_class::code(ostream& s, CgenEnvironment env) {
    std::vector<Expression> args;
    for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
        args.push_back(actual->nth(i));
    }
    
    for (size_t i = 0; i < args.size(); ++i) {
        args[i]->code(s, env);
        emit_push_stack(ACC, s);
        env.reserve_temp_slot();
    }
    
    expr->code(s, env);
    
    int label_not_void = next_label();
    emit_bne(ACC, ZERO, label_not_void, s);
    
    Symbol filename = env.m_current_class_node->get_filename();
    emit_load_string(ACC, stringtable.lookup_string(filename->get_string()), s);
    emit_load_imm(T1, 1, s);
    emit_jal("_dispatch_abort", s);
    
    emit_label_def(label_not_void, s);
    
    string class_name_str = type_name->get_string();
    string dispatch_tab_label = class_name_str + DISPTAB_SUFFIX;
    emit_load_address(T1, (char*)dispatch_tab_label.c_str(), s);
    
    CgenNode* target_node = g_class_table_ptr->get_class_node(type_name);
    int method_idx = target_node->get_dispatch_idx_map()[name];
    
    emit_load(T1, method_idx, T1, s);
    emit_jalr(T1, s);
    
    for (size_t i = 0; i < args.size(); ++i) env.release_temp_slot();
}

void dispatch_class::code(ostream& s, CgenEnvironment env) {
    std::vector<Expression> args;
    for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
        args.push_back(actual->nth(i));
    }

    for (size_t i = 0; i < args.size(); ++i) {
        args[i]->code(s, env);
        emit_push_stack(ACC, s);
        env.reserve_temp_slot();
    }
    
    expr->code(s, env);
    
    int label_not_void = next_label();
    emit_bne(ACC, ZERO, label_not_void, s);
    
    Symbol filename = env.m_current_class_node->get_filename();
    emit_load_string(ACC, stringtable.lookup_string(filename->get_string()), s);
    emit_load_imm(T1, 1, s);
    emit_jal("_dispatch_abort", s);
    
    emit_label_def(label_not_void, s);
    
    Symbol type = expr->get_type();
    Symbol class_name = (type == SELF_TYPE) ? env.m_current_class_node->name : type;
    CgenNode* node = g_class_table_ptr->get_class_node(class_name);
    int method_idx = node->get_dispatch_idx_map()[name];
    
    emit_load(T1, 2, ACC, s);
    emit_load(T1, method_idx, T1, s);
    emit_jalr(T1, s);
    
    for (size_t i = 0; i < args.size(); ++i) env.release_temp_slot();
}

void cond_class::code(ostream& s, CgenEnvironment env) {
    pred->code(s, env);
    emit_fetch_int_attr(T1, ACC, s);
    
    int label_false = next_label();
    int label_end = next_label();
    
    emit_beq(T1, ZERO, label_false, s);
    then_exp->code(s, env);
    emit_branch(label_end, s);
    
    emit_label_def(label_false, s);
    else_exp->code(s, env);
    emit_label_def(label_end, s);
}

void loop_class::code(ostream& s, CgenEnvironment env) {
    int label_start = next_label();
    int label_end = next_label();
    
    emit_label_def(label_start, s);
    pred->code(s, env);
    
    emit_fetch_int_attr(T1, ACC, s);
    emit_beq(T1, ZERO, label_end, s);
    
    body->code(s, env);
    emit_branch(label_start, s);
    
    emit_label_def(label_end, s);
    emit_move(ACC, ZERO, s);
}

struct CaseSorter {
    bool operator()(Case a, Case b) {
        branch_class* b1 = (branch_class*)a;
        branch_class* b2 = (branch_class*)b;
        CgenNode* n1 = g_class_table_ptr->get_class_node(b1->type_decl);
        CgenNode* n2 = g_class_table_ptr->get_class_node(b2->type_decl);
        return n1->get_inheritance_path().size() > n2->get_inheritance_path().size();
    }
};

void typcase_class::code(ostream& s, CgenEnvironment env) {
    expr->code(s, env);
    
    int label_not_void = next_label();
    emit_bne(ACC, ZERO, label_not_void, s);
    
    Symbol filename = env.m_current_class_node->get_filename();
    emit_load_string(ACC, stringtable.lookup_string(filename->get_string()), s);
    emit_load_imm(T1, 1, s);
    emit_jal("_case_abort2", s);
    
    emit_label_def(label_not_void, s);
    emit_load(T1, 0, ACC, s);
    
    std::vector<Case> cases_vec;
    for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
        cases_vec.push_back(cases->nth(i));
    }
    std::sort(cases_vec.begin(), cases_vec.end(), CaseSorter());

    int label_end = next_label();
    for (size_t i = 0; i < cases_vec.size(); ++i) {
        branch_class* b = (branch_class*)cases_vec[i];
        int label_next_case = next_label();
        
        CgenNode* type_node = g_class_table_ptr->get_class_node(b->type_decl);
        int min_tag = type_node->m_class_tag;
        int max_tag = type_node->m_max_child_tag;
        
        emit_blti(T1, min_tag, label_next_case, s);
        emit_bgti(T1, max_tag, label_next_case, s);
        
        env.enter_scope();
        env.add_local_var(b->name);
        emit_push_stack(ACC, s);
        
        b->expr->code(s, env);
        
        emit_addiu(SP, SP, 4, s);
        env.exit_scope();
        emit_branch(label_end, s);
        
        emit_label_def(label_next_case, s);
    }
    
    emit_jal("_case_abort", s);
    emit_label_def(label_end, s);
}

void block_class::code(ostream& s, CgenEnvironment env) {
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        body->nth(i)->code(s, env);
    }
}

void let_class::code(ostream& s, CgenEnvironment env) {
    if (init->get_type() == No_type) {
        if (type_decl == Int) emit_load_int(ACC, inttable.lookup_string("0"), s);
        else if (type_decl == Str) emit_load_string(ACC, stringtable.lookup_string(""), s);
        else if (type_decl == Bool) emit_load_bool(ACC, BoolConst(0), s);
        else emit_move(ACC, ZERO, s);
    } else {
        init->code(s, env);
    }
    
    env.enter_scope();
    env.add_local_var(identifier);
    emit_push_stack(ACC, s);
    
    body->code(s, env);
    
    emit_addiu(SP, SP, 4, s);
    env.exit_scope();
}

void plus_class::code(ostream& s, CgenEnvironment env) {
    e1->code(s, env);
    emit_push_stack(ACC, s);
    env.reserve_temp_slot();
    
    e2->code(s, env);
    emit_jal("Object.copy", s);
    
    emit_load(T1, 1, SP, s);
    emit_addiu(SP, SP, 4, s);
    env.release_temp_slot();
    
    emit_fetch_int_attr(T1, T1, s);
    emit_fetch_int_attr(T2, ACC, s);
    emit_add(T3, T1, T2, s);
    emit_store_int_attr(T3, ACC, s);
}

void sub_class::code(ostream& s, CgenEnvironment env) {
    e1->code(s, env);
    emit_push_stack(ACC, s);
    env.reserve_temp_slot();
    
    e2->code(s, env);
    emit_jal("Object.copy", s);
    
    emit_load(T1, 1, SP, s);
    emit_addiu(SP, SP, 4, s);
    env.release_temp_slot();
    
    emit_fetch_int_attr(T1, T1, s);
    emit_fetch_int_attr(T2, ACC, s);
    emit_sub(T3, T1, T2, s);
    emit_store_int_attr(T3, ACC, s);
}

void mul_class::code(ostream& s, CgenEnvironment env) {
    e1->code(s, env);
    emit_push_stack(ACC, s);
    env.reserve_temp_slot();
    
    e2->code(s, env);
    emit_jal("Object.copy", s);
    
    emit_load(T1, 1, SP, s);
    emit_addiu(SP, SP, 4, s);
    env.release_temp_slot();
    
    emit_fetch_int_attr(T1, T1, s);
    emit_fetch_int_attr(T2, ACC, s);
    emit_mul(T3, T1, T2, s);
    emit_store_int_attr(T3, ACC, s);
}

void divide_class::code(ostream& s, CgenEnvironment env) {
    e1->code(s, env);
    emit_push_stack(ACC, s);
    env.reserve_temp_slot();
    
    e2->code(s, env);
    emit_jal("Object.copy", s);
    
    emit_load(T1, 1, SP, s);
    emit_addiu(SP, SP, 4, s);
    env.release_temp_slot();
    
    emit_fetch_int_attr(T1, T1, s);
    emit_fetch_int_attr(T2, ACC, s);
    emit_div(T3, T1, T2, s);
    emit_store_int_attr(T3, ACC, s);
}

void neg_class::code(ostream& s, CgenEnvironment env) {
    e1->code(s, env);
    emit_jal("Object.copy", s);
    
    emit_fetch_int_attr(T1, ACC, s);
    emit_neg(T1, T1, s);
    emit_store_int_attr(T1, ACC, s);
}

void lt_class::code(ostream& s, CgenEnvironment env) {
    e1->code(s, env);
    emit_push_stack(ACC, s);
    env.reserve_temp_slot();
    
    e2->code(s, env);
    emit_load(T1, 1, SP, s);
    emit_addiu(SP, SP, 4, s);
    env.release_temp_slot();
    
    emit_fetch_int_attr(T1, T1, s);
    emit_fetch_int_attr(T2, ACC, s);
    
    emit_load_bool(ACC, BoolConst(1), s);
    int label = next_label();
    emit_blt(T1, T2, label, s);
    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(label, s);
}

void eq_class::code(ostream& s, CgenEnvironment env) {
    e1->code(s, env);
    emit_push_stack(ACC, s);
    env.reserve_temp_slot();
    
    e2->code(s, env);
    emit_move(T2, ACC, s);
    
    emit_load(T1, 1, SP, s);
    emit_addiu(SP, SP, 4, s);
    env.release_temp_slot();
    
    int label_true = next_label();
    int label_end = next_label();
    
    emit_beq(T1, T2, label_true, s);
    
    emit_load_bool(ACC, BoolConst(1), s);
    emit_load_bool(A1, BoolConst(0), s);
    emit_jal("equality_test", s);
    emit_branch(label_end, s);
    
    emit_label_def(label_true, s);
    emit_load_bool(ACC, BoolConst(1), s);
    emit_label_def(label_end, s);
}

void leq_class::code(ostream& s, CgenEnvironment env) {
    e1->code(s, env);
    emit_push_stack(ACC, s);
    env.reserve_temp_slot();
    
    e2->code(s, env);
    emit_load(T1, 1, SP, s);
    emit_addiu(SP, SP, 4, s);
    env.release_temp_slot();
    
    emit_fetch_int_attr(T1, T1, s);
    emit_fetch_int_attr(T2, ACC, s);
    
    emit_load_bool(ACC, BoolConst(1), s);
    int label = next_label();
    emit_bleq(T1, T2, label, s);
    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(label, s);
}

void comp_class::code(ostream& s, CgenEnvironment env) {
    e1->code(s, env);
    emit_fetch_int_attr(T1, ACC, s);
    
    int label = next_label();
    emit_load_bool(ACC, BoolConst(1), s);
    emit_beqz(T1, label, s);
    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(label, s);
}

void int_const_class::code(ostream& s, CgenEnvironment env) {
    emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
}

void string_const_class::code(ostream& s, CgenEnvironment env) {
    emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
}

void bool_const_class::code(ostream& s, CgenEnvironment env) {
    emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream& s, CgenEnvironment env) {
    if (type_name == SELF_TYPE) {
        emit_load_address(T1, CLASSOBJTAB, s);
        emit_load(T2, 0, SELF, s);
        emit_sll(T2, T2, 3, s);
        emit_addu(T1, T1, T2, s);
        emit_push_stack(T1, s);
        
        emit_load(ACC, 0, T1, s);
        emit_jal("Object.copy", s);
        
        emit_load(T1, 1, SP, s);
        emit_addiu(SP, SP, 4, s);
        emit_load(T1, 1, T1, s);
        emit_jalr(T1, s);
    } else {
        string proto = type_name->get_string();
        proto += PROTOBJ_SUFFIX;
        emit_load_address(ACC, (char*)proto.c_str(), s);
        emit_jal("Object.copy", s);
        
        string init = type_name->get_string();
        init += CLASSINIT_SUFFIX;
        emit_jal((char*)init.c_str(), s);
    }
}

void isvoid_class::code(ostream& s, CgenEnvironment env) {
    e1->code(s, env);
    emit_move(T1, ACC, s);
    
    emit_load_bool(ACC, BoolConst(1), s);
    int label = next_label();
    emit_beqz(T1, label, s);
    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(label, s);
}

void no_expr_class::code(ostream& s, CgenEnvironment env) {
    emit_move(ACC, ZERO, s);
}

void object_class::code(ostream& s, CgenEnvironment env) {
    if (name == self) {
        emit_move(ACC, SELF, s);
        return;
    }
    
    int offset = -1;
    if ((offset = env.find_var_offset(name)) != -1) {
        emit_load(ACC, env.next_temp_offset() - offset, SP, s);
    } else if ((offset = env.find_param_offset(name)) != -1) {
        emit_load(ACC, offset + 3, FP, s);
    } else if ((offset = env.find_attrib_offset(name)) != -1) {
        emit_load(ACC, offset + DEFAULT_OBJFIELDS, SELF, s);
    }
}