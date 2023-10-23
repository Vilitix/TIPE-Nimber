#include<stdio.h>
#include<stdlib.h>
#include<stdbool.h>
#include<assert.h>

void jeu_de_nim (int nombre_objet){
    int choix;
    int joueur = 0;
    while(1<nombre_objet){
        printf("joueur %d à toi de jouer choisi le nombre d'objet a retirer \n", joueur);
        scanf("%d", &choix);
        nombre_objet = nombre_objet - choix;
        joueur = (joueur + 1) % 2;
        printf("il reste %d objets\n", nombre_objet);
        for(int j= 0; j<nombre_objet; j++){
        printf("|");
        }
        printf("\n");
    }
    if (nombre_objet == 0){
        printf("Bravo joueur %d tu as gagné\n", joueur);
    }
    else{printf("Bravo joueur %d tu as gagné\n", (joueur + 1) % 2);}

}

int main(){
int n;
printf("choisi le nombre d'objet pour jouer\n");
scanf("%d", &n);
jeu_de_nim(n);
}