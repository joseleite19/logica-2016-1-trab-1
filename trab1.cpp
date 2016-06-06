#include <bits/stdc++.h>
using namespace std;

#define INF 1000000009
#define CONJUNCAO 1
#define NAO_DIJUNCAO 13
#define NAO_IMPLICACAO 14
#define BI_IMPLICACAO 2
#define DUPLANEGACAO 20

#define DISJUNCAO 3
#define NAO_CONJUNCAO 11
#define IMPLICACAO 4
#define NAO_BI_IMPLICACAO 12

map<char, int> priori;
set<char> operadores;
string apagaParentesesIni(string s);
string botaParenteses(string s, bool pode);
bool pode(string &s);
string neg(string form);

class formula{
	int pos;
public:
	string form;
	int oper;
	int achaOp(){
		int nivel, id, nivelAtual, id_priori;
		nivelAtual = 0;
		id = -1;
		nivel = id_priori = INF;
		bool neg = false;
		if(form[0] == '!' && form[1] == '!'){
			pos = 1;
			return oper = 20;
		}
		
		for(int i = 0; form[i]; i++){
			if(form[i] == ' ') continue;
			else if(form[i] == '(') nivelAtual++;
			else if(form[i] == ')') nivelAtual--;
			else if(is_operator(form[i])){
				if(nivelAtual < nivel || (nivelAtual == nivel && prioridade(form[i]) <= id_priori)){
					id_priori = prioridade(form[i]);
					nivel = nivelAtual;
					id = i;
				}
			}
		}

		if(form[id] == '!'){
			neg = true;
			int id2 = -1;
			nivelAtual = 0;
			nivel = id_priori = INF;
			for(int i = 0; form[i]; i++){
				if(form[i] == ' ' || i == id) continue;
				else if(form[i] == '(') nivelAtual++;
				else if(form[i] == ')') nivelAtual--;
				else if(is_operator(form[i])){
					if(nivelAtual < nivel || (nivelAtual == nivel && prioridade(form[i]) <= id_priori)){
						id_priori = prioridade(form[i]);
						nivel = nivelAtual;
						id2 = i;
					}
				}
			}
			id = id2;
		}
		pos = id;
		if(id == -1) return oper = -1;
		int operacao = 0;
		if(form[id] == '&') operacao = CONJUNCAO;
		if(form[id] == '|') operacao = DISJUNCAO;
		if(form[id] == '>') operacao = IMPLICACAO;
		if(form[id] == '<') operacao = BI_IMPLICACAO;
		if(form[id] == '!') return oper = DUPLANEGACAO;
		if(neg) operacao += 10;
		return oper = (operacao == 10 ? 0 : operacao);
	}

	pair<string, string> operandos(){
		string first, second;
		if(oper == DUPLANEGACAO){
			first = form.substr(pos+1);
			second = "";
		}
		else if(oper == -1){
			first = second = "";
		}
		else{
			first = form.substr(0, pos);
			second = form.substr(pos+1);
		}


		int nivel, fim_termo, inicio_termo;
		
		/* apaga parenteses excessivos do primeiro operando */
		nivel = 0;
		inicio_termo = 0;
		for(int i = first.length() - 1; i >= 0; i--){
			if(first[i] == '(') nivel++;
			if(first[i] == ')') nivel--;
			if(nivel == 1){
				inicio_termo = i+1;
				break;
			}
		}

		first = first.substr(inicio_termo, first.length() - inicio_termo);
		
		/* apaga parenteses excessivos do segundo operando */
		nivel = 0;
		fim_termo = second.length();
		for(int i = 0; i < second.length(); i++){
			if(second[i] == '(') nivel++;
			if(second[i] == ')') nivel--;
			if(nivel == -1){
				fim_termo = i;
				break;
			}
		}
		second = second.substr(0, fim_termo);

		
		first = apagaParentesesIni(first);
		second = apagaParentesesIni(second);
		
		return make_pair(first, second);
	}

	bool is_alpha(){
		if(oper == CONJUNCAO) return true;
		if(oper == NAO_DIJUNCAO) return true;
		if(oper == NAO_IMPLICACAO) return true;
		if(oper == BI_IMPLICACAO) return true;
		if(oper == DUPLANEGACAO) return true;
		return false;
	}

	int prioridade(char c){
		return priori[c];
	}

	bool is_operator(char c){
		return operadores.find(c) != operadores.end();
	}
	const char *form_c(){
		return form.c_str();
	}

	formula(string f = ""){
		string s = "";
		for(int i = 0; f[i]; i++){
			if(f[i] == ' ') continue;
			else if(f[i] == '-'){
				if(f[i+1] == '>'){
					s += '>';
					i++;
				}
				else s += '!';
			}

			else if(f[i] == '<' && f[i+1] == '-' && f[i+2] == '>'){
				s += '<';
				i += 2;
			}
			else s += f[i];
		}
		form = apagaParentesesIni(s);
		achaOp();
	}
	void show(){
		for(int i = 0; i < form.length(); i++){
			if(form[i] == '!') printf("-");
			else if(form[i] == '>') printf(" -> ");
			else if(form[i] == '<') printf(" <-> ");
			else if(form[i] == '&') printf(" & ");
			else if(form[i] == '|') printf(" | ");
			else printf("%c", form[i]);
		}
	}
};

struct tree{
private:
	set<string> s;
	bool closed;
	void fechaRamos(bool fechado, set<string> & conj){
		if(closed) return;
		if(!esq && !dir){
			if(fechado || conj.find(neg(phi.form)) != conj.end()){
				esq = new tree("X");
				closed = true;
			}
		}
		else{
			if(!fechado){
				if(conj.find(neg(phi.form)) != conj.end()){
					closed = true;
					if(esq) esq->fechaRamos(true, conj);
					if(dir) dir->fechaRamos(true, conj);
				}
				else{
					conj.insert(phi.form);
					closed = true;
					
					if(esq){
						esq->fechaRamos(false, conj);
						closed = (closed && esq->closed);
					}
					if(dir){
						dir->fechaRamos(false, conj);
						closed = (closed && dir->closed);
					}

					conj.erase(phi.form);
				}
			}
			else{
				closed = true;
				if(esq) esq->fechaRamos(true, conj);
				if(dir) dir->fechaRamos(true, conj);
			}
		}
	}
public:
	tree *esq, *dir;
	formula phi;

	bool alpha;

	tree(string s);

	void insere(pair<string, string> form, bool filho_do_outro){
		if(closed) return;
		bool tem_filho = false;
		if(form.first == phi.form){ form.first = form.second; form.second = ""; }
		if(form.second == phi.form) form.second = "";

		if(esq){
			esq->insere(form, filho_do_outro);
			tem_filho = true;
		}
		if(dir){
			dir->insere(form, filho_do_outro);
			tem_filho = true;
		}
		if(!tem_filho){
			if(form.first != "") esq = new tree(form.first);
			if(form.second != ""){
				if(filho_do_outro) esq->insere(make_pair(form.second, ""), false);
				else dir = new tree(form.second);
			}
		}
	}

	void fechaRamos(){
		s.clear();
		fechaRamos(false, s);
	}

	bool taFechada(){
		return closed;
	}

	void show(){
		phi.show();
		printf(", [");
		if(esq) esq->show();
		printf("], [");
		if(dir) dir->show();
		printf("]");
	}

	void showtab(){
		printf("[%s,", closed ? "SIM" : "NAO");
		show();
		printf("]\n");
	}

	~tree(){
		delete esq;
		delete dir;
	}
};
struct comp{
	bool operator()(tree *a, tree *b){
		return !(*a).alpha && (*b).alpha;
	}
};
priority_queue<tree*, vector<tree*>, comp>pq;

/* declarado depois para não ter conflito com a fila de prioridade */
tree::tree(string s){
	esq = dir = NULL;
	closed = false;
	phi = formula(s);
	alpha = phi.is_alpha();
	pq.push(this);
}

void inicializeOperators(){
	priori['!'] = 10;
	priori['&'] = 9;
	priori['|'] = 8;
	priori['>'] = 7;
	priori['<'] = 6;

	operadores.insert('!');
	operadores.insert('&');
	operadores.insert('|');
	operadores.insert('>');
	operadores.insert('<');
}

string apagaParentesesIni(string s){
	while(1){
		bool canErase = false;
		bool first = true;
		int nivel = 0;
		if(s[0] != '(') break;
		for(int i = 0; i < s.length(); i++){
			if(s[i] == '('){
				nivel++;
				first = false;
			}
			if(s[i] == ')') nivel--;
			if(nivel == 0 && !first){
				if(i == s.length()-1) canErase = true;
				else canErase = false;
				break;
			}
		}
		if(!canErase) break;
		s = s.substr(1, s.length() - 2);
	}
	return s;
}
string neg(string form){
	if(form[0] == '!')
		return apagaParentesesIni(form.substr(1));
	return '!' + botaParenteses(form, pode(form));
}

string botaParenteses(string s, bool pode){
	return pode ? "(" + s + ")" : s;
}

bool pode(string &s){
	int nivel = 0, id = 0;
	if(s.length() > 2 && (s[0] == '(' || (s[0] == '!' && s[1] == '('))){
		for(int i = 0; i < s.length(); i++){
			if(s[i] == '(') nivel++;
			if(s[i] == ')'){
				nivel--;
				if(nivel == 0){
					id = i+1;
					break;
				}
			}
		}
	}

	for(int i = id; i < s.length(); i++)
		if(s[i] != '!' && operadores.find(s[i]) != operadores.end())
			return true;
	return false;
}

pair<string, string> criaFormulasParaRegraDeInferencia(formula &f, bool &filho_do_outro){
	const int & operacao = f.oper;
	filho_do_outro = f.is_alpha();
	pair<string, string> ret = f.operandos();
	bool podeParenteses1, podeParenteses2;

	podeParenteses1 = podeParenteses2 = false;

	podeParenteses1 = pode(ret.first);
	podeParenteses2 = pode(ret.second);


	if(operacao == NAO_DIJUNCAO){
		ret.first =  "!" + botaParenteses(ret.first, podeParenteses1);
		ret.second = "!" + botaParenteses(ret.second, podeParenteses2);
	}
	else if(operacao == NAO_IMPLICACAO){
		ret.second = "!" + botaParenteses(ret.second, podeParenteses2);
	}
	else if(operacao == BI_IMPLICACAO){
		string aux1, aux2;
		aux1 = botaParenteses(ret.first, podeParenteses1) + ">" + botaParenteses(ret.second, podeParenteses2);
		aux2 = botaParenteses(ret.second, podeParenteses2) + ">" + botaParenteses(ret.first, podeParenteses1);
		ret.first = aux1;
		ret.second = aux2;
	}
	else if(operacao == NAO_CONJUNCAO){
		ret.first =  "!" + botaParenteses(ret.first, podeParenteses1);
		ret.second = "!" + botaParenteses(ret.second, podeParenteses2);
	}
	else if(operacao == IMPLICACAO){
		ret.first =  "!" + botaParenteses(ret.first, podeParenteses1);
	}
	else if(operacao == NAO_BI_IMPLICACAO){
		string aux1, aux2;
		aux1 = "!(" + botaParenteses(ret.first, podeParenteses1) + ">" + botaParenteses(ret.second, podeParenteses2) + ")";
		aux2 = "!(" + botaParenteses(ret.second, podeParenteses2) + ">" + botaParenteses(ret.first, podeParenteses1) + ")";
		ret.first = aux1;
		ret.second = aux2;
	}
	return ret;
}

void aplicaRegradeInferencia(tree *t){
	bool filho_do_outro;
	pair<string, string> ret = criaFormulasParaRegraDeInferencia(t->phi, filho_do_outro);
	t->insere(ret, filho_do_outro);
}

int main(){
	inicializeOperators();

	string s;

	while(getline(cin, s)){
		while(!pq.empty()) pq.pop();
		
		tree arvore(neg(s));

		while(!pq.empty()){
			tree *p = pq.top(); pq.pop();
			if(p->phi.oper != -1){ /* se pode aplicar alguma regra de inferencia */

				aplicaRegradeInferencia(p);

				arvore.fechaRamos();
				if(arvore.taFechada()) break;
			}
		}
		arvore.showtab();
	}
	
	return 0;
}