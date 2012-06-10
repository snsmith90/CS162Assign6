all: minijs

minijs: syntax.scala values.scala domains.scala interpreter.scala gc.scala
	scalac -d build -unchecked syntax.scala values.scala domains.scala interpreter.scala gc.scala

clean:
	rm -rf build/*
