# Project Name and Purpose: Projectile - To predict whether a launched projectile hits its target.

build: Projectile.jar

Projectile.jar: Projectile/InputFormat.java Projectile/InputConstraints.java Projectile/Calculations.java Projectile/OutputFormat.java
	javac Projectile/InputFormat.java Projectile/InputConstraints.java Projectile/Calculations.java Projectile/OutputFormat.java
	jar -cvf Projectile.jar Projectile

doc: doxConfig Projectile/InputFormat.java Projectile/InputConstraints.java Projectile/Calculations.java Projectile/OutputFormat.java
	doxygen doxConfig

.PHONY: build doc
