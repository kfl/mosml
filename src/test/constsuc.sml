(* 32-bit architecture: *)

val minInt1 = ~1073741824;
val maxInt1 =  1073741823;

val minInt2  = ~0x40000000;
val maxInt21 =  0x3fffffff;
val maxInt22 =  0x3FFFFFFF;

val test1 = minInt1 = minInt2;
val test2 = maxInt1 = maxInt21 andalso maxInt21 = maxInt22;

val maxWord1 = 0w2147483647;
val maxWord2 = 0wx7fffffff;

val test3 = maxWord1 = maxWord2 andalso maxWord1 = Word.fromInt ~1;

val maxWord8_1 = 0w255;
val maxWord8_2 = 0wxFF;

val test4 = maxWord8_1 = maxWord8_2 andalso maxWord8_1 = Word.fromInt 255;
