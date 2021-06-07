build: HelloWorld/HelloWorld.class

HelloWorld/HelloWorld.class: HelloWorld/HelloWorld.java HelloWorld/Helper.java
	javac HelloWorld/HelloWorld.java

run: build
	java HelloWorld.HelloWorld $(RUNARGS)

.PHONY: build run
