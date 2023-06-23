# Project Name and Purpose: Projectile - To predict whether a launched projectile hits its target.

build: Projectile/Projectile.class

Projectile/Projectile.class: Projectile/Projectile.java
	javac Projectile/Projectile.java

run: build
	java Projectile.Projectile $(RUNARGS)

doc: doxConfig Projectile/Projectile.java
	doxygen doxConfig

.PHONY: build run doc
