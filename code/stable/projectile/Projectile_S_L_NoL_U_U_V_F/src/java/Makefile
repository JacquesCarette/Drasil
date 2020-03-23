build: Projectile.jar

Projectile.jar: Projectile/InputFormat.java Projectile/InputConstraints.java Projectile/OutputFormat.java Projectile/Calculations.java
	javac Projectile/InputFormat.java Projectile/InputConstraints.java Projectile/OutputFormat.java Projectile/Calculations.java
	jar -cvf Projectile.jar Projectile

doc: doxConfig Projectile/InputFormat.java Projectile/InputConstraints.java Projectile/OutputFormat.java Projectile/Calculations.java
	doxygen doxConfig

.PHONY: build doc
