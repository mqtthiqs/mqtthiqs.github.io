import java.util.*;

class tp4 {
  public static void main(String[] args) {
    PhoneBook c = new PhoneBook();
    c.addPerson("Paul", "McCartney", "0123456789");
    c.addPerson("John", "Lennon", "0600000000");
    c.addCompany("Abbey Road Studios", "0611111111");
    System.out.print("Quel numéro cherchez-vous? ");
    String s = new Scanner(System.in).nextLine();
    System.out.print("Ce numéro appartient à ");
    System.out.println(c.searchByPhone(s));
  }
}

class PhoneBook {
  Contact[] contacts = new Contact[100];
  int next=0;

  public String searchByPhone(String phone) {
    for (int i=0; i<next; i++)
      if (contacts[i].getPhone().equals(phone))
        return contacts[i].getIdent();
    return null;
  }

  public void addPerson(String lastname, String firstname, String phone) {
    contacts[next] = new Person(lastname, firstname, phone);
    next++;
  }

  public void addCompany(String name, String phone) {
    contacts[next] = new Company(name, phone);
    next++;
  }
}

interface Contact {
  String getPhone();
  String getIdent();
}

class Company implements Contact {
  String name;
  String phone;

  public String getIdent() { return name; }
  public String getPhone() { return phone; }

  public Company(String name, String phone) {
    this.name = name;
    this.phone = phone;
  }
}

class Person implements Contact {
  String lastname;
  String firstname;
  String phone;

  public String getIdent() { return lastname + " " + firstname; }
  public String getPhone() { return phone; }

  public Person(String lastname, String firstname, String phone) {
    this.lastname = lastname;
    this.firstname = firstname;
    this.phone = phone;
  }
}
