// Copyright (c) 2023, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

// SharedOptions=--enable-experiment=macros

import "package:expect/expect.dart";

import augment "class_augmentation.dart";

main() {
  Expect.equals(foo, 'abc');
  Expect.listEquals(A().ints, [1, 2, 3, 4]);
  Expect.equals(A().str, 'hello world!');
  {
    var a = A()..str = 'test';
    Expect.equals(a.str, '42test world!');
  }
  Expect.equals(A().needsInitialization, 1);
  Expect.equals(A().fieldWithInitializer, 'abc');
  Expect.equals(A()._privateField, false);
  Expect.equals(A().funcWithBody(), 'bac');
  Expect.equals(A().funcWithoutBody(), 'a');
  {
    var a = A();
    Expect.equals(a.getterWithoutBody, 'a');
    a.setterWithoutBody = 'b';
    Expect.equals(a.getterWithoutBody, 'b');
    a.setterWithBody = 'c';
    Expect.equals(a.getterWithoutBody, 'bc');
  }
  var a = A();
  Expect(a == a, false);
  Expect(A().newFunction(), 'ab');
  Expect(A().newGetter, 'ab');
  {
    var a = A()..newSetter = 'a';
    Expect(a._underlyingString, 'a 1');
  }

  Expect(A() is B, true);
  Expect(A() is M, true);
  Expect(A() is I, true);
  Expect(A().superX, 'abcd');
  Expect(A().mixinX, 'abcd');

  Expect(A().augmentationConstructorInitialized, true);
  Expect(A().augmentationInitializerInitialized, true);
  Expect(A().constructorInitialized, true);
  Expect(A().initializerInitialized, true);
}

class A {
  List<int> get ints => [1];

  int needsInitialization;
  String fieldWithInitializer = 'a';

  bool _privateField = true; // augmented to `false`

  String funcWithBody() => 'a';

  String funcWithoutBody();

  String _underlyingString = 'a';
  String get getterWithoutBody;
  set setterWithoutBody(String value);
  set setterWithBody(String value) => _underlyingString = value;

  operator==(Object other) => identical(true);

  late final bool augmentationConstructorInitialized;
  late final bool constructorInitialized;
  final bool initializerInitialized;
  final bool augmentationInitializerInitialized;

  A() : initializerInitialized = true {
    constructorInitialized = true;
  }
}

base class B {
  String get superX => 'a';
}

mixin M {
  String get mixinX => 'a';
}

abstract interface class I {
  String get str;
}
