#include <bits/stdc++.h>
using namespace std;

// Simple TISC assembler for the problem's .dmp syntax.
// Two-pass: collect labels and data addresses, then resolve expressions and emit ints.

struct Item {
    // raw tokens of an item expression in infix form
    vector<string> tokens; // supports unary '-', parentheses, '+', '-', ids, numbers, '?'
    int addressIndex = -1; // address in output (memory index) where this item will be placed
    vector<string> leadingLabels; // labels defined before this item (label:)*
};

struct Instr {
    string opcode; // "msubleq","rsubleq","ldorst" or "."
    vector<Item> items; // for '.' and for operands
    int addressIndex = -1; // address of opcode in Mem (for non-dot), or -1 for '.'
    int expandedItemCount = 0; // after arg expansion for msubleq/rsubleq
};

static bool isIdentStart(char c){ return c=='_' || isalpha((unsigned char)c); }
static bool isIdentChar(char c){ return c=='_' || isalnum((unsigned char)c); }

struct Asm {
    vector<Instr> instrs; // sequence as appears
    // label -> address
    unordered_map<string,int> labelAddr;
};

// Trim helpers
static string trim(const string &s){ size_t i=0,j=s.size(); while(i<j && isspace((unsigned char)s[i])) ++i; while(j>i && isspace((unsigned char)s[j-1])) --j; return s.substr(i,j-i);}    

static bool startsWith(const string &s,const string &p){ return s.rfind(p,0)==0; }

// Tokenize a line into opcode/labels and items; handle labels before tokens (label:)
// The grammar allows labels before opcode and before items. We'll parse as:
// [labels:]* opcode (items each may have leading labels:)* ';' or '.' data list

// Split by ';' first, ignore empty.

vector<string> tokenize(const string& s){
    vector<string> t; string cur; auto flush=[&]{ if(!cur.empty()){ t.push_back(cur); cur.clear(); } }; 
    for(size_t i=0;i<s.size();++i){ char c=s[i];
        if(isspace((unsigned char)c)){ flush(); continue; }
        if(c==';' || c==':' || c=='(' || c==')' || c=='+' || c=='-' || c==',' ){
            flush(); string one(1,c); t.push_back(one); continue;
        }
        cur.push_back(c);
    }
    flush();
    return t;
}

// Parse a number; returns true if ok.
static bool parseInt(const string& s, long long &v){
    if(s.empty()) return false;
    char* end=nullptr; errno=0; long long x=strtoll(s.c_str(), &end, 10);
    if(errno!=0 || end==s.c_str() || *end!='\0') return false;
    v=x; return true;
}

// Shunting-yard to evaluate expression with + and -, unary -, parentheses, operands: number, id, '?'
struct EvalCtx{
    const unordered_map<string,int>* labelAddr;
    int qAddress; // value of '?' for current item (its address+1) – pass in during evaluation
};

static long long evalExprTokens(const vector<string>& toks, size_t l, size_t r, const EvalCtx& ctx){
    // Convert to RPN
    vector<string> output; vector<string> op;
    auto precedence=[&](const string& s){ if(s=="+"||s=="-") return 1; if(s=="u-") return 2; return 0; };
    // Handle unary minus: replace '-' that follows start or '(' or operator with 'u-'
    vector<string> nt;
    for(size_t i=l;i<r;i++) nt.push_back(toks[i]);
    vector<string> pt;
    for(size_t i=0;i<nt.size();++i){
        string tk=nt[i];
        if(tk=="-"){
            if(i==0){ pt.push_back("u-"); continue; }
            string prev=pt.back();
            if(prev=="("||prev=="+"||prev=="-"||prev=="u-") pt.push_back("u-"); else pt.push_back("-");
        }else pt.push_back(tk);
    }
    for(size_t i=0;i<pt.size();++i){
        string tk=pt[i];
        if(tk=="+"||tk=="-"||tk=="u-"){
            while(!op.empty()){
                string top=op.back();
                if((top=="+"||top=="-"||top=="u-") && precedence(top)>=precedence(tk)){
                    output.push_back(top); op.pop_back();
                }else break;
            }
            op.push_back(tk);
        } else if(tk=="("){
            op.push_back(tk);
        } else if(tk==")"){
            while(!op.empty() && op.back()!="(") { output.push_back(op.back()); op.pop_back(); }
            if(!op.empty() && op.back()=="(") op.pop_back();
        } else {
            output.push_back(tk);
        }
    }
    while(!op.empty()){ output.push_back(op.back()); op.pop_back(); }

    // Evaluate RPN
    vector<long long> st;
    for(const string& tk: output){
        if(tk=="+"||tk=="-"){
            if(st.size()<2) throw runtime_error("bad expr");
            long long b=st.back(); st.pop_back(); long long a=st.back(); st.pop_back();
            st.push_back(tk=="+"? a+b : a-b);
        } else if(tk=="u-"){
            if(st.empty()) throw runtime_error("bad expr");
            st.back() = -st.back();
        } else if(tk=="?"){
            st.push_back(ctx.qAddress);
        } else {
            long long v;
            if(parseInt(tk,v)) st.push_back(v);
            else {
                auto it=ctx.labelAddr->find(tk);
                if(it==ctx.labelAddr->end()) throw runtime_error("unknown label: "+tk);
                st.push_back(it->second);
            }
        }
    }
    if(st.size()!=1) throw runtime_error("bad expr end");
    return st.back();
}

int main(){
    ios::sync_with_stdio(false); cin.tie(nullptr);
    vector<string> lines; string line;
    while(true){
        string s; if(!getline(cin,s)) break; lines.push_back(s); }

    Asm as;
    // First pass: parse lines into Instr and collect labels with provisional addresses by simulating memory layout
    int curAddr = 0; // memory index
    for(size_t li=0; li<lines.size(); ++li){
        string raw = trim(lines[li]);
        if(raw.empty()) continue;
        // strip comments: everything after //
        size_t com = raw.find("//"); if(com!=string::npos) raw = trim(raw.substr(0,com));
        if(raw.empty()) continue;
        // Tokenize
        vector<string> tk = tokenize(raw);
        if(tk.empty()) continue;

        // consume leading labels: sequences of id ':'
        size_t idx=0;
        while(idx+1<tk.size() && tk[idx] != "." && tk[idx] != ";" && tk[idx] != ","){
            // If pattern id ':' appears, record label pointing to current next address position
            if(idx+1<tk.size() && tk[idx+1]==":" && isIdentStart(tk[idx][0])){
                as.labelAddr[tk[idx]] = curAddr; idx += 2; continue;
            }
            break;
        }
        if(idx>=tk.size()) continue;
        if(tk[idx]==";") continue; // stray semicolon

        Instr ins; ins.opcode = tk[idx]; ++idx;
        bool isDot = (ins.opcode == ".");

        // Parse items until ';' by consuming one expression per item: term [(+|-) term]
        function<Item(size_t&)> parseItem = [&](size_t &p){
            Item it;
            // skip leading labels in item: label:*
            while(p+1<tk.size() && tk[p+1]==":" && isIdentStart(tk[p][0])){ it.leadingLabels.push_back(tk[p]); p+=2; }
            auto parseTerm = [&](size_t &q, auto &&parseTermRef) -> void {
                if(q>=tk.size()) return; 
                if(tk[q]=="-") { it.tokens.push_back("-"); ++q; parseTermRef(q, parseTermRef); return; }
                if(tk[q]=="("){
                    int depth=0; // include balanced parentheses tokens
                    do{
                        it.tokens.push_back(tk[q]);
                        if(tk[q]=="(") depth++;
                        else if(tk[q]==")") depth--;
                        ++q;
                        if(q>=tk.size()) break;
                    } while(depth>0);
                    return;
                }
                // id, number, or '?'
                it.tokens.push_back(tk[q]); ++q; 
            };

            // first term
            parseTerm(p, parseTerm);
            // optional + or - then second term
            if(p<tk.size() && (tk[p]=="+"||tk[p]=="-")){
                it.tokens.push_back(tk[p]); ++p;
                parseTerm(p, parseTerm);
            }
            return it;
        };

        while(idx<tk.size() && tk[idx] != ";"){ 
            Item it = parseItem(idx); 
            if(!it.tokens.empty()) ins.items.push_back(it);
        }
        // consume ';' if present
        // expansion of msubleq/rsubleq: if <3 items, auto-extend
        if(!isDot){
            int n = (int)ins.items.size();
            if(ins.opcode=="msubleq" || ins.opcode=="rsubleq"){
                if(n==1){ // A -> A A ?
                    Item B = ins.items[0];
                    Item C; C.tokens = {"?"};
                    ins.items.push_back(B); ins.items.push_back(C);
                } else if(n==2){
                    Item C; C.tokens = {"?"}; ins.items.push_back(C);
                }
            }
            // Now assign opcode address and item addresses
            ins.addressIndex = curAddr; curAddr += 1; // opcode slot
            for(size_t i=0;i<ins.items.size();++i){ ins.items[i].addressIndex = curAddr; curAddr += 1; }
        } else {
            // '.' items occupy memory but no opcode slot
            for(size_t i=0;i<ins.items.size();++i){ ins.items[i].addressIndex = curAddr; curAddr += 1; }
        }
        as.instrs.push_back(move(ins));
    }

    // Second pass: evaluate expressions and emit
    // Before evaluation, register item-level labels to their resolved addresses
    for(auto &ins : as.instrs){
        for(auto &it : ins.items){
            for(auto &lab : it.leadingLabels){ as.labelAddr[lab] = it.addressIndex; }
        }
    }
    vector<long long> mem(curAddr, 0);
    for(auto &ins : as.instrs){
        if(ins.opcode != "."){
            int opv = (ins.opcode=="msubleq"?0: ins.opcode=="rsubleq"?1: ins.opcode=="ldorst"?2: -9999);
            if(opv==-9999){ cerr << "Unknown opcode: "<<ins.opcode<<"\n"; return 0; }
            mem[ins.addressIndex] = opv;
        }
        for(auto &it : ins.items){
            EvalCtx ctx; ctx.labelAddr = &as.labelAddr; ctx.qAddress = it.addressIndex + 1; // '?' is addr+1
            try{
                long long val = evalExprTokens(it.tokens, 0, it.tokens.size(), ctx);
                mem[it.addressIndex] = val;
            } catch(const exception& e){
                cerr << "Expr error: ";
                for(auto &tk: it.tokens) cerr << tk << ' ';
                cerr << "| at addr "<< it.addressIndex << " msg: " << e.what() << "\n";
                return 1;
            }
        }
    }

    // Print in logical groups: each instruction as 4 numbers per line; consecutive '.' data grouped on a single line.
    for(size_t i=0;i<as.instrs.size();){
        auto &ins = as.instrs[i];
        if(ins.opcode != "."){
            // ensure three operands (expanded for msubleq/rsubleq)
            cout << mem[ins.addressIndex];
            // print exactly 3 operands if available; missing ones shouldn't happen now
            for(int k=0;k<3 && k < (int)ins.items.size(); ++k){
                cout << ' ' << mem[ins.items[k].addressIndex];
            }
            cout << "\n";
            ++i;
        } else {
            // gather consecutive '.' items
            bool first = true;
            while(i<as.instrs.size() && as.instrs[i].opcode=="."){
                for(auto &it : as.instrs[i].items){
                    if(!first) cout << ' ';
                    cout << mem[it.addressIndex];
                    first = false;
                }
                ++i;
            }
            cout << "\n";
        }
    }
    return 0;
}
