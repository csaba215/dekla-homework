#include "cekla.h"// így szabályos C++ program is

int pow2(const int A, const int N, const int P) {
	if (N > 0)
		return pow2(A, N-1, P*A);
	else
		return P;
}

const list reverse2(const list L, const list L0) {
	if (L == nil) return L0;
	return reverse2(tl(L), cons(hd(L), L0));
}

const list felbont2(const int szam,const int szamrendszer,const list lista){
	if(szam == 0)
		return lista;
	int darab = szam%szamrendszer;
	const list l = cons(darab,lista);
	return felbont2((szam-darab)/szamrendszer,szamrendszer,l);
}

int pow(const int A, const int N){
	return pow2(A, N, 1);
}

const list reverse(const list L) {
	return reverse2(L,nil);
}

const list felbont(const int szam,const int szamrendszer){
	return felbont2(szam,szamrendszer,nil);
}

int vissza2(const list L,const int A, const int sum,const int n){
	if(L == nil)
		return sum;
	return vissza2(tl(L),A,sum+hd(L)*pow(A,n),n+1);
}

int vissza(const list L,const int A){
	return vissza2(L,A,0,0);
}

const list bont(const list l1,const list l2,const int n){
	if(l1 == nil)
		return l2;
	if(n%2==0)
		return bont(tl(l1),cons(hd(l1),l2),n+1);
	return bont(tl(l1),l2,n+1);
}

const list osszefuz(const list paratlan_forditva,const list paros){
	if(paratlan_forditva == nil)
		return paros;
	return osszefuz(tl(paratlan_forditva),cons(hd(paratlan_forditva),paros));
}


int atrendezett(const int S, const int A) {
	const list lista = felbont(S,A);
	const list paros_forditva = bont(lista,nil,1);
	const list paratlan_forditva = bont(lista,nil,0);
	const list paros = reverse(paros_forditva);
	const list atrendezett = reverse(osszefuz(paratlan_forditva,paros));
	return vissza(atrendezett,A);
}


int main() {// szabályos függvénydeklaráció
	writeln(atrendezett(123, 10));
	writeln(atrendezett(101, 10));
	writeln(atrendezett(5, 2));
	writeln(atrendezett(1023, 10));
	writeln(atrendezett(11, 3));
	writeln(atrendezett(162738495, 10));
	writeln(atrendezett(15263748, 10));
}




