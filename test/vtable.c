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
static const struct table_Monster table_Monster_for_Monster = { Monster_whoami, Monster_grow };

// Constructor
void Monster_Constructor(struct Monster* this, char *n, float s) {
  this->vtbl = &table_Monster_for_Monster;
  this->name = n;
  this->size = s;
}

int main() {
  // struct Monster *Frank = (struct Monster *)malloc((int)sizeof(struct Monster));
  struct Monster Frank;
  Monster_Constructor(&Frank, "Frank", 42); // manually call constructor
  Frank.vtbl->grow(&Frank, 1);
  Frank.vtbl->whoami(&Frank);
}