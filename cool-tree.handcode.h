#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <iostream>
#include <vector>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"

// 上下文环境类声明
class CgenEnvironment;

// ---------------------------------------------------------
// 辅助工具函数声明 (将实现移至 cgen.cc 以修复链接错误)
// ---------------------------------------------------------
Boolean copy_Boolean(Boolean b);
void assert_Boolean(Boolean);
void dump_Boolean(ostream& stream, int padding, Boolean b);

void dump_Symbol(ostream& stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

// --- AST 节点扩展宏 ---

#define Program_EXTRAS \
    virtual void cgen(ostream&) = 0; \
    virtual void dump_with_types(ostream&, int) = 0;

#define program_EXTRAS \
    void cgen(ostream&); \
    void dump_with_types(ostream&, int);

#define Class__EXTRAS \
    virtual Symbol get_name() = 0; \
    virtual Symbol get_parent() = 0; \
    virtual Symbol get_filename() = 0; \
    virtual void dump_with_types(ostream&, int) = 0;

#define class__EXTRAS \
    Symbol get_name() { return name; } \
    Symbol get_parent() { return parent; } \
    Symbol get_filename() { return filename; } \
    void dump_with_types(ostream&, int);

#define Feature_EXTRAS \
    virtual void dump_with_types(ostream&, int) = 0; \
    virtual bool is_method() const = 0;

#define Feature_SHARED_EXTRAS \
    void dump_with_types(ostream&, int);

#define attr_EXTRAS \
    bool is_method() const { return false; }

#define method_EXTRAS \
    bool is_method() const { return true; }

#define Formal_EXTRAS \
    virtual void dump_with_types(ostream&, int) = 0;

#define formal_EXTRAS \
    void dump_with_types(ostream&, int);

#define Case_EXTRAS \
    virtual void dump_with_types(ostream&, int) = 0;

#define branch_EXTRAS \
    void dump_with_types(ostream&, int);

#define Expression_EXTRAS \
    Symbol type; \
    Symbol get_type() { return type; } \
    Expression set_type(Symbol s) { type = s; return this; } \
    virtual void code(ostream&, CgenEnvironment) = 0; \
    virtual void dump_with_types(ostream&, int) = 0; \
    void dump_type(ostream&, int); \
    Expression_class() { type = (Symbol)NULL; }

#define Expression_SHARED_EXTRAS \
    void code(ostream&, CgenEnvironment); \
    void dump_with_types(ostream&, int);

#endif