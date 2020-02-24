build: Projectile/Control.class

Projectile/Control.class: Projectile/Control.java Projectile/InputParameters.java Projectile/OutputFormat.java Projectile/Calculations.java
	javac Projectile/Control.java

run: build
	java Projectile.Control $(RUNARGS)

doc: doxConfig Projectile/Control.java Projectile/InputParameters.java Projectile/OutputFormat.java Projectile/Calculations.java
	doxygen doxConfig

.PHONY: build run doc
