#ifndef _COOL_CGEN_H_
#define _COOL_CGEN_H_

#include <assert.h>
#include <stdio.h>
#include <vector>
#include <map>
#include <algorithm>
#include <utility> // for std::pair
#include <string>  // Fixed: 引入 string

#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness { Basic, NotBasic };

#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenEnvironment {
private:
    std::vector<std::pair<Symbol, int> > m_scope_stack;
    std::map<Symbol, int> m_param_map;
    int m_current_temp_offset;

public:
    CgenNode* m_current_class_node;

    CgenEnvironment() : m_current_temp_offset(0), m_current_class_node(NULL) {}

    void enter_scope() {
        m_scope_stack.push_back(std::make_pair((Symbol)NULL, 0));
    }

    void exit_scope() {
        while (!m_scope_stack.empty()) {
            std::pair<Symbol, int> top = m_scope_stack.back();
            m_scope_stack.pop_back();
            if (top.first == NULL) break;
            m_current_temp_offset--;
        }
    }

    int find_var_offset(Symbol sym) {
        for (int i = m_scope_stack.size() - 1; i >= 0; --i) {
            if (m_scope_stack[i].first == sym) {
                return m_scope_stack[i].second;
            }
        }
        return -1;
    }

    int find_param_offset(Symbol sym) {
        if (m_param_map.find(sym) != m_param_map.end()) {
            return m_param_map[sym];
        }
        return -1;
    }

    int find_attrib_offset(Symbol sym); 

    void add_local_var(Symbol sym) {
        m_scope_stack.push_back(std::make_pair(sym, m_current_temp_offset));
        m_current_temp_offset++;
    }

    void add_param(Symbol sym, int offset) {
        m_param_map[sym] = offset;
    }

    void reserve_temp_slot() {
        m_current_temp_offset++;
    }

    void release_temp_slot() {
        m_current_temp_offset--;
    }

    int next_temp_offset() const { return m_current_temp_offset; }
};

class CgenClassTable : public SymbolTable<Symbol, CgenNode> {
private:
    List<CgenNode> *m_nodes;
    ostream& m_str;

    int m_tag_string;
    int m_tag_int;
    int m_tag_bool;

    std::vector<CgenNode*> m_all_nodes;
    std::map<Symbol, int> m_class_tag_table;

    void assign_class_tags(CgenNode* node, int& counter);
    void build_inheritance_tree();
    void set_parent_child_relations(CgenNodeP node);

    void emit_global_data();
    void emit_gc_select();
    void emit_constants();
    void emit_class_name_tab();
    void emit_class_obj_tab();
    void emit_dispatch_tables();
    void emit_prot_objs();
    
    void emit_global_text();
    void emit_class_inits();
    void emit_class_methods();

public:
    CgenClassTable(Classes classes, ostream& str);
    
    void code();
    CgenNodeP root();
    CgenNodeP get_class_node(Symbol name) { return probe(name); }
    std::vector<CgenNode*> get_all_nodes() { return m_all_nodes; }
    std::map<Symbol, int>& get_tag_map() { return m_class_tag_table; }
};

class CgenNode : public class__class {
private:
    CgenNodeP m_parent;
    List<CgenNode> *m_children;
    Basicness m_basic_status;

    std::vector<CgenNode*> m_inheritance_chain;
    std::vector<attr_class*> m_layout_attribs;
    std::vector<method_class*> m_layout_methods;

    std::map<Symbol, int> m_dispatch_idx_map;
    std::map<Symbol, Symbol> m_dispatch_def_map;
    std::map<Symbol, int> m_attrib_idx_map;

public:
    int m_class_tag;
    int m_max_child_tag;

    // Fixed: 使用 Class_ 而不是 Class_c
    CgenNode(Class_ c, Basicness bstatus, CgenClassTableP class_table);

    void add_child(CgenNodeP child);
    List<CgenNode>* get_children() { return m_children; }
    
    void set_parent(CgenNodeP p);
    
    // Fixed: 改名为 get_parentnd 避免与 class__class::get_parent() 冲突
    CgenNodeP get_parentnd() { return m_parent; }
    
    bool is_basic() { return (m_basic_status == Basic); }

    void emit_prot_obj(ostream& s);
    void emit_init_ref(ostream& s);
    void emit_method_defs(ostream& s);

    std::vector<CgenNode*> get_inheritance_path();
    std::vector<attr_class*> get_full_attributes(); 
    std::vector<method_class*> get_full_methods(); 

    std::map<Symbol, int>& get_dispatch_idx_map() { return m_dispatch_idx_map; }
    std::map<Symbol, Symbol>& get_dispatch_def_map() { return m_dispatch_def_map; }
    std::map<Symbol, int>& get_attrib_idx_map() { return m_attrib_idx_map; }
};

class BoolConst {
private:
    int val;
public:
    BoolConst(int);
    void code_def(ostream&, int boolclasstag);
    void code_ref(ostream&) const;
};

#endif