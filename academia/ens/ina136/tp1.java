import java.util.Scanner;

class tp1
{
  // 1.1.
  public static void main11(String[] args) {
    int monNombrePrefere = 42;
    System.out.print("Mon premier nombre est ");
    System.out.println(monNombrePrefere);
    Scanner sc = new Scanner(System.in);
    System.out.print("Un nombre? ");
    int i = sc.nextInt();
    System.out.print("Un autre? ");
    int j = sc.nextInt();
    System.out.print("La somme est ");
    System.out.println(monNombrePrefere+i+j);
  }

  // 1.2.
  // Je vois alors des sauts à la ligne après chaque invite à
  // taper un nombres

  // 1.3.
  public static void main13(String[] args) {
    System.out.print("Rayon? ");
    double r = new Scanner(System.in).nextDouble();
    System.out.println("Périmètre: "+2*Math.PI*r);
    System.out.println("Aire "+Math.PI*r*r);
  }

  // 1.4.
  public static void main14(String[] args) {
    System.out.print("Nombre 1? ");
    int n1 = new Scanner(System.in).nextInt();
    System.out.print("Nombre 2? ");
    int n2 = new Scanner(System.in).nextInt();
    System.out.print("Nombre 3? ");
    int n3 = new Scanner(System.in).nextInt();
    System.out.println("moyenne: "+(n1+n2+n3)/3);
  }

  // 1.5
  // En remplaçant int par double, on calcule la moyenne en flottant;
  // sinon, on arrondissait à l'entier inférieur

  // 2.1 int, 4
  // 2.2 bool, true
  // 2.3 bool, false
  // 2.4 bool, (varie en fonction de i et j)
  // 2.5 bool, true
  // 2.6 erreur de type (on mélange int et bool)

  // 3.1
  public static void main31(String[] args) {
    int secret = 42;
    System.out.print("Nombre? ");
    int n = new Scanner(System.in).nextInt();
    if (n < secret) System.out.println("plus petit");
    else if (n > secret) System.out.println("plus grand");
    else System.out.println("egal");
  }

  // 3.2
  public static void main32(String[] args) {
    System.out.println("Que souhaitez-vous faire?");
    System.out.println("1. Afficher \"Hello world\"");
    System.out.println("2. Afficher \"Bye Bye\"");
    System.out.println("3. Demander trois nombres à l’utilisateur et afficher leur moyenne");
    int n = new Scanner(System.in).nextInt();
    if (n == 1) System.out.println("Hello world");
    else if (n == 2) System.out.println("Bye Bye");
    else if (n == 3) {
      System.out.print("Nombre 1? ");
      int n1 = new Scanner(System.in).nextInt();
      System.out.print("Nombre 2? ");
      int n2 = new Scanner(System.in).nextInt();
      System.out.print("Nombre 3? ");
      int n3 = new Scanner(System.in).nextInt();
      System.out.println("moyenne: "+(n1+n2+n3)/3);
    }
    else System.out.println("Choix invalide");
  }

  // 4.1
  public static void main41(String[] args) {
    System.out.print("Nombre? ");
    int n = new Scanner(System.in).nextInt();
    for (int i=0; i<n; i++) System.out.println(n);
  }


  // 4.2
  public static void main42(String[] args) {
    System.out.print("Nombre de notes? ");
    int n = new Scanner(System.in).nextInt();
    double somme = 0.0;
    for (int i=0; i<n; i++) {
      System.out.print("Note "+i+"? ");
      somme += new Scanner(System.in).nextInt();
    }
    System.out.println("moyenne: "+somme/n);
  }


  // 4.3
  public static void main43(String[] args) {
    int secret = 42;
    int n = -1;
    while (n != secret) {
      System.out.print("Nombre? ");
      n = new Scanner(System.in).nextInt();
      if (n < secret) System.out.println("plus petit");
      else if (n > secret) System.out.println("plus grand");
      else System.out.println("egal");
    }
  }

  // 4.4
  public static void main44(String[] args) {
    int n;
    do {
      System.out.println("Que souhaitez-vous faire?");
      System.out.println("1. Afficher \"Hello world\"");
      System.out.println("2. Afficher \"Bye Bye\"");
      System.out.println("3. Demander trois nombres à l’utilisateur et afficher leur moyenne");
      System.out.println("4. Quitter");
      n = new Scanner(System.in).nextInt();
      if (n == 1) System.out.println("Hello world");
      else if (n == 2) System.out.println("Bye Bye");
      else if (n == 3) {
        System.out.print("Nombre 1? ");
        int n1 = new Scanner(System.in).nextInt();
        System.out.print("Nombre 2? ");
        int n2 = new Scanner(System.in).nextInt();
        System.out.print("Nombre 3? ");
        int n3 = new Scanner(System.in).nextInt();
        System.out.println("moyenne: "+(n1+n2+n3)/3);
      }
      else if (n != 4) System.out.println("Choix invalide");
    } while (n != 4);
  }

  // 4.5
  public static void main45(String[] args) {
    int n = 10;
    for (int i=0; i<n; i++) {
      for (int j=0; j<i*2+1; j++)
        System.out.print("*");
      System.out.println();
    }
  }

  // 4.6
  public static void main(String[] args) {
    int n=10;
    for (int i=0; i<n; i++) {
      for (int j=0; j<10-i; j++) {
        System.out.print(" ");
      }
      for (int j=0; j<i*2+1; j++) {
        System.out.print("*");
      }
      System.out.println();
    }
  }
}

