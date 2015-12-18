#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

// Traits
struct Monster {
  char *name;
  float size;
};

// Constructor
void Monster_Constructor(struct Monster* this, char *n, float s) {
  this->name = n;
  this->size = s;
}

// Inheritance
struct Zombie {
  struct Monster monster;
  float age;
};

void Zombie_Constructor(struct Zombie *this, char *n, float s, float a) {
  // manually call base classes constructors first
  Monster_Constructor(&this->monster, n, s);
  // then, additional fields;
  this->age = a;
}

int main() {
  // struct Monster *Frank = (struct Monster *)malloc((int)sizeof(struct Monster));
  struct Monster Frank;
  Monster_Constructor(&Frank, "Frank", 42);
  printf("%s\n", Frank.name);
  printf("%f\n", Frank.size);
  struct Zombie Gordon;
  Zombie_Constructor(&Gordon, "Gordon", 50, 51);
  printf("%s\n", Gordon.monster.name);
  printf("%f\n", Gordon.monster.size);
  printf("%f\n", Gordon.age);
}
