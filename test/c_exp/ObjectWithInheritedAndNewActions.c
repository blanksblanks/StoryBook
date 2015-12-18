#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

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

// Inheritance
struct Zombie;
struct table_Zombie {
  // OVERLOADED FUNCTIONS ARE NOT DECLARED HERE!!!!!!!!!!
  // OR YOU WILL GET SEGFAULTS
  void (*cry)(struct Zombie *this);
};

struct Zombie {
  // Note superclasses have to be at the top lest segfaults
  struct Monster monster;
  const struct table_Zombie *vtbl;
  float age;
};

// Override the function
void Zombie_whoami(struct Zombie *this) {
  printf("My name is %s, my size is %f and my age is %f\n", this->monster.name, this->monster.size, this->age);
}

// Re-cast
void ZombieMonster_whoami(struct Monster *this) {
  Zombie_whoami((struct Zombie*) this);
}

// actions Zombie inherited from Monster
static const struct table_Monster table_Monster_for_Zombie = { ZombieMonster_whoami, Monster_grow };

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

void Zombie_Constructor(struct Zombie *this, char *n, float s, float a) {
  Monster_Constructor(&this->monster, n, s);
  // Override virtual function pointers that have just been initialized by
  // Zombie constructor with our own version. We can override any virtual
  // function in the base class ^_^
  this->monster.vtbl = &table_Monster_for_Zombie;
  this->vtbl = &table_Zombie_for_Zombie;
  this->age = a;
}

int main() {
  // struct Monster *Frank = (struct Monster *)malloc((int)sizeof(struct Monster));
  struct Monster Frank;
  Monster_Constructor(&Frank, "Frank", 42);
  struct Zombie Gordon;
  Zombie_Constructor(&Gordon, "Gordon", 50, 51);
  // Inherited actions!!
  Frank.vtbl->whoami(&Frank);
  Gordon.monster.vtbl->whoami(&Gordon.monster);
  Gordon.monster.vtbl->grow(&Gordon.monster, 5);
  Gordon.monster.vtbl->whoami(&Gordon.monster);
  // New action specific to Zombie!
  Gordon.vtbl->cry(&Gordon);
}
