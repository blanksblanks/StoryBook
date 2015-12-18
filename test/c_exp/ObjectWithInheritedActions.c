#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

// Vtable struct
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

// Monster's Vtable which lists all its virtual functions
static const struct table_Monster table_Monster_for_Monster = { Monster_whoami };

// Constructor
void Monster_Constructor(struct Monster* this, char *n, float s) {
  this->vtbl = &table_Monster_for_Monster;
  this->name = n;
  this->size = s;
}

struct Zombie {
  struct Monster monster;
  float age;
};

// The Zombie version of the function we want to override
void Zombie_whoami(struct Zombie *this) {
  printf("My name is %s, my size is %f and my age is %f\n", this->monster.name, this->monster.size, this->age);
}

// We need to cast from Monster to Zombie to use Zombie version
void ZombieMonster_whoami(struct Monster *this) {
  Zombie_whoami((struct Zombie*) this);
}

// Enumerates all virtual functions for Zombie inherited from Monster
static const struct table_Monster table_Monster_for_Zombie = { ZombieMonster_whoami, Monster_grow };

void Zombie_Constructor(struct Zombie *this, char *n, float s, float a) {
  printf("Zombie constructor was called\n");
  Monster_Constructor(&this->monster, n, s);
  // Override virtual function pointers that have just been initialized by
  // Zombie constructor with our own version. We can override any virtual
  // function in the base class!
  this->monster.vtbl = &table_Monster_for_Zombie;
  printf("this->monster.vtbl in constructor was called\n");
  this->age = a;
}

int main() {
  // struct Monster *Frank = (struct Monster *)malloc((int)sizeof(struct Monster));
  // Init Characters
  struct Monster Frank;
  Monster_Constructor(&Frank, "Frank", 42);
  struct Zombie Gordon;
  Zombie_Constructor(&Gordon, "Gordon", 50, 51);
  // Call Actions
  Frank.vtbl->whoami(&Frank);
  Gordon.monster.vtbl->whoami(&Gordon.monster);
  Gordon.monster.vtbl->grow(&Gordon.monster, 5);
  Zombie_whoami(&Gordon);
}
