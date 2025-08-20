import java.util.Scanner;

class tp2
{
  // 1.1.
  public static void main11(String[] args) {
    System.out.print("x? ");
    int x = new Scanner(System.in).nextInt();
    System.out.print("y? ");
    int y = new Scanner(System.in).nextInt();
    int acc = 1;
    for (;y>0; y--)
      acc *= x;
    System.out.println("x^y = "+acc);
  }

  // 1.2.
  public static void main12(String[] args) {
    System.out.print("Cours du dollar (valeur de 1 dollar)? ");
    double cours = new Scanner(System.in).nextDouble();
    double euros;
    do {
      System.out.print("Somme en euros? ");
      euros = new Scanner(System.in).nextDouble();
      if (euros > 0) System.out.println("La somme en dollars: "+ euros / cours);
    } while (euros > 0);
  }

  // 1.3.
  public static void main13(String[] args) {
    System.out.print("Table? ");
    int n = new Scanner(System.in).nextInt();
    for (int i=0; i<10; i++)
      System.out.println(i+" * "+n+" = "+i*n);
  }

  // 1.5.
  public static void main15(String[] args) {
    for (int i=0; i<10; i++) {
      for (int j=0; j<10; j++) {
        if (i*j<10) System.out.print(" ");
        System.out.print(i*j+" ");
      }
      System.out.println();
    }
  }

  // 2.1.
  public static void main21(String[] args) {
    int[] tab = {12, 15, 13, 10, 8, 9, 13, 14};
     for (int i=0; i<tab.length; i++) {
       System.out.println(tab[i]);
    }
  }

  // 2.2.
  public static void main22(String[] args) {
    int[] tab = {12, 15, 13, 10, 8, 9, 13, 14};
    int max = 0;
     for (int i=0; i<tab.length; i++)
       if (tab[i] > max) max = tab[i];
     System.out.println(max);
  }

  // 2.3.
  public static void main23(String[] args) {
    int[] tab = {12, 15, 13, 10, 8, 9, 13, 14};
    System.out.print("Entier? ");
    int n = new Scanner(System.in).nextInt();
    boolean present = false;
     for (int i=0; i<tab.length; i++)
       if (tab[i] == n) present = true;
     if (present) System.out.println("présent");
     else System.out.println("absent");
  }

  // 2.4.
  public static void main24(String[] args) {
    int[] tab = {12, 15, 13, 10, 8, 9, 13, 14};
    System.out.print("Entier? ");
    int n = new Scanner(System.in).nextInt();
    boolean present = false;
     for (int i=0; i<tab.length; i++)
       if (tab[i] == n) {
         present = true;
         System.out.println("présent à l'indice "+i);
       }
     if (!present) System.out.println("absent");
  }

  // 2.5.
  public static void main25(String[] args) {
    int[] tab = {12, 15, 13, 10, 8, 9, 13, 14};
    System.out.print("Entier? ");
    int n = new Scanner(System.in).nextInt();
    int dernier_indice = -1;
     for (int i=0; i<tab.length; i++)
       if (tab[i] == n)
         dernier_indice = i;
     if (dernier_indice > 0) System.out.println("présent à l'indice "+dernier_indice);
     else System.out.println("absent");
  }

  // 2.6.
  public static void main26(String[] args) {
    System.out.print("Nombre de cases? ");
    int n = new Scanner(System.in).nextInt();
    int[] tab = new int[n];
    for (int i=0; i<n; i++) {
      System.out.print("Cases "+i+"? ");
      tab[i] = new Scanner(System.in).nextInt();
    }
  }

  // 2.7.
  // ce programme met dans chaque case du tableau bat l'indice de son
  // occurence dans tab. Donc bat = {1, 0, 6, 4, 7, 3, 2, 5}
  public static void main(String[] args) {
    int[] tab = {1, 0, 6, 5, 3, 7, 2, 4};
    int[] bat = new int[8];
    for (int i=0; i<8; i++)
      bat[tab[i]] = i;
    // affichage de bat
    for (int i=0; i<8; i++)
      System.out.println(bat[i]);
  }
}
