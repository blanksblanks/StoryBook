#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

// static void print_function(const char *func, char *n, void *this_) {
//   printf("%s created %s at address %p\n", func, n, this_);
// }

// Vtable
struct Monster;
struct table_Monster {
  void (*whoami)(struct Monster *this);
  void (*grow)(struct Monster *this, float s);
};

// Traits
struct Monster {
  const struct table_Monster *vtbl;
  char *name;
  float size;
};

// Actions
void Monster_whoami(struct Monster *this) {
  printf("My name is %s and my size is %f\n", this->name, this->size);
}

void Monster_grow(struct Monster *this, float sz) {
  this->size += sz;
}

// the global, static table data for Monster
// this structure enumerates all virtual functions of Monster
static const struct table_Monster table_Monster_for_Monster = { Monster_whoami };

// Constructor
void Monster_Constructor(struct Monster* this, char *n, float s) {
  this->vtbl = &table_Monster_for_Monster;
  this->name = n;
  this->size = s;
}

// Superclass #2
struct Human;
struct table_Human {
  void (*whoami)(struct Human *this);
};

struct Human {
  const struct table_Human *vtbl;
  char *gender;
};

void Human_whoami(struct Human *this) {
  printf("I am %s\n", this->gender);
}

static const struct table_Human table_Human_for_Human = { Human_whoami };

void Human_Constructor(struct Human* this, char *x) {
  this->vtbl = &table_Human_for_Human;
  this->gender = x;
}

// Multiple Inheritance
struct Zombie;
struct table_Zombie {
  // OVERLOADED FUNCTIONS LIKE WHOAMI ARE NOT DECLARED HERE!!!!!!!!!!
  // OR YOU WILL GET SEGFAULTS
  void (*cry)(struct Zombie *this);
};

struct Zombie {
  // Note superclasses have to be at the top lest segfaults
  struct Monster monster;
  struct Human human;
  const struct table_Zombie *vtbl;
  float age;
};

// Override the function
void Zombie_whoami(struct Zombie *this) {
  printf("My name is %s, my size is %f, my gender is %s and my age is %f\n", this->monster.name, this->monster.size, this->human.gender, this->age);
}

// Re-cast
void ZombieMonster_whoami(struct Monster *this) {
  Zombie_whoami((struct Zombie*) this);
}

// actions Zombie inherited from Monster
static const struct table_Monster table_Monster_for_Zombie = { ZombieMonster_whoami, Monster_grow };

// This is crazy, but we know that 'this' is the second struct
// Zombie inherits from, so we have to fix the pointer by an offset
void ZombieHuman_whoami(struct Human *this) {
  size_t offset = (size_t) &((struct Zombie*) NULL)->human;
  Zombie_whoami((struct Zombie*) ( ( (char*) this ) - offset ) );
}

static const struct table_Human table_Human_for_Zombie = { ZombieHuman_whoami };

// Add a new action for Zombie
void Zombie_cry(struct Zombie *this) {
  printf("%s is sad, wah wah wah\n", this->monster.name);
}

void ZombieMonster_cry(struct Zombie *this) {
  printf("cry gets called inside\n");
  size_t offset = (size_t) &((struct Zombie*) NULL)->monster;
  Zombie_cry((struct Zombie*) ( ( (char*) this) - offset ) );
}

// add the new action for Zombie's vtable
static const struct table_Zombie table_Zombie_for_Zombie = { Zombie_cry };

void Zombie_Constructor(struct Zombie *this, char *n, float s, char *x, float a) {
  Monster_Constructor(&this->monster, n, s);
  Human_Constructor(&this->human, x);
  // Override virtual function pointers that have just been initialized by
  // Zombie constructor with our own version. We can override any virtual
  // function in the base class ^_^
  this->monster.vtbl = &table_Monster_for_Zombie;
  this->human.vtbl = &table_Human_for_Zombie;
  this->vtbl = &table_Zombie_for_Zombie;
  this->age = a;
}

int main() {
  // struct Monster *Frank = (struct Monster *)malloc((int)sizeof(struct Monster));
  struct Monster Frank;
  Monster_Constructor(&Frank, "Frank", 42);
  struct Zombie Greta;
  Zombie_Constructor(&Greta, "Greta", 50, "female", 51);
  // Overwritten actions from Monster and Human classes!!
  Greta.monster.vtbl->whoami(&Greta.monster);
  Greta.human.vtbl->whoami(&Greta.human);
  // Inherited and unchanged action!
  Greta.monster.vtbl->grow(&Greta.monster, 5);
  // The same overwritten actions! Same output!
  Greta.human.vtbl->whoami(&Greta.human);
  Greta.human.vtbl->whoami(&Greta.human);
  // New action specific to Zombie!
  Greta.vtbl->cry(&Greta);
}
