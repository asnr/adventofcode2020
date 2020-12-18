{-# LANGUAGE OverloadedStrings #-}

module Input where

import qualified Data.Text as T

example :: T.Text
example = "light red bags contain 1 bright white bag, 2 muted yellow bags.\n\
\dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\
\bright white bags contain 1 shiny gold bag.\n\
\muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\
\shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\
\dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\
\vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\
\faded blue bags contain no other bags.\n\
\dotted black bags contain no other bags."

examplePart2 :: T.Text
examplePart2 = "shiny gold bags contain 2 dark red bags.\n\
\dark red bags contain 2 dark orange bags.\n\
\dark orange bags contain 2 dark yellow bags.\n\
\dark yellow bags contain 2 dark green bags.\n\
\dark green bags contain 2 dark blue bags.\n\
\dark blue bags contain 2 dark violet bags.\n\
\dark violet bags contain no other bags."

puzzle :: T.Text
puzzle = "bright olive bags contain 5 dotted white bags, 2 wavy lavender bags.\n\
\plaid brown bags contain 3 bright lime bags, 5 plaid coral bags.\n\
\dim coral bags contain 1 shiny maroon bag, 2 bright orange bags, 3 clear lavender bags.\n\
\dull olive bags contain 5 wavy purple bags, 2 plaid white bags, 4 vibrant magenta bags, 1 light brown bag.\n\
\dim yellow bags contain 1 bright cyan bag, 3 striped green bags, 5 dim white bags.\n\
\drab chartreuse bags contain 3 shiny lime bags, 2 bright indigo bags, 2 muted yellow bags, 5 dim tan bags.\n\
\dim cyan bags contain 4 drab tomato bags, 3 dotted teal bags, 1 posh purple bag, 2 faded brown bags.\n\
\striped indigo bags contain 2 wavy brown bags.\n\
\dull coral bags contain 5 mirrored salmon bags.\n\
\light violet bags contain 3 light maroon bags, 3 plaid purple bags, 1 pale lime bag, 2 pale orange bags.\n\
\clear magenta bags contain 3 muted tan bags, 3 clear lime bags, 3 bright magenta bags, 3 faded purple bags.\n\
\drab turquoise bags contain 1 dark violet bag.\n\
\dim black bags contain 4 vibrant turquoise bags.\n\
\dotted gray bags contain 1 posh salmon bag, 5 drab lime bags, 1 clear coral bag, 1 faded lime bag.\n\
\light lime bags contain 5 wavy lavender bags, 3 shiny black bags.\n\
\bright yellow bags contain 3 dotted beige bags, 4 clear purple bags, 4 faded salmon bags, 5 drab black bags.\n\
\shiny teal bags contain 3 dotted chartreuse bags, 1 wavy yellow bag, 3 clear lavender bags.\n\
\shiny crimson bags contain 2 light plum bags, 3 shiny black bags.\n\
\shiny lime bags contain 1 dim turquoise bag, 4 pale fuchsia bags.\n\
\pale lavender bags contain 3 bright lavender bags, 5 wavy blue bags.\n\
\shiny purple bags contain no other bags.\n\
\clear lime bags contain 2 drab bronze bags.\n\
\vibrant lavender bags contain 2 plaid crimson bags, 3 plaid yellow bags.\n\
\clear gray bags contain 4 shiny indigo bags, 1 vibrant salmon bag, 3 bright yellow bags, 2 light green bags.\n\
\dark lime bags contain 1 light lime bag, 3 vibrant yellow bags, 5 faded bronze bags.\n\
\bright chartreuse bags contain 5 mirrored olive bags, 2 mirrored white bags, 1 bright black bag, 2 clear olive bags.\n\
\wavy red bags contain 5 bright black bags, 4 dotted salmon bags, 4 clear tomato bags, 4 faded teal bags.\n\
\posh coral bags contain 4 posh plum bags, 4 shiny magenta bags, 4 light plum bags.\n\
\shiny coral bags contain 5 shiny yellow bags.\n\
\light bronze bags contain 2 shiny purple bags, 5 mirrored maroon bags, 2 light white bags.\n\
\mirrored olive bags contain 5 mirrored white bags.\n\
\pale crimson bags contain 2 vibrant gold bags, 1 bright yellow bag, 2 light green bags, 4 shiny gray bags.\n\
\bright cyan bags contain 1 muted plum bag, 5 dark bronze bags, 4 dotted teal bags, 5 faded fuchsia bags.\n\
\posh bronze bags contain 1 muted gold bag, 5 shiny turquoise bags, 3 wavy bronze bags.\n\
\mirrored yellow bags contain 1 dull maroon bag, 2 posh orange bags, 3 striped lime bags, 4 plaid crimson bags.\n\
\muted brown bags contain 5 mirrored indigo bags.\n\
\posh lavender bags contain 2 dotted violet bags, 3 clear orange bags.\n\
\shiny salmon bags contain 3 bright red bags, 1 light turquoise bag, 2 drab turquoise bags.\n\
\striped teal bags contain no other bags.\n\
\pale purple bags contain 1 faded yellow bag, 2 dotted beige bags, 1 dark red bag, 5 plaid yellow bags.\n\
\drab tomato bags contain 2 faded aqua bags, 2 faded purple bags.\n\
\wavy yellow bags contain 2 wavy gold bags, 1 dotted beige bag, 2 shiny gold bags, 4 posh tomato bags.\n\
\shiny yellow bags contain 3 wavy brown bags, 5 dull violet bags, 2 muted tomato bags, 4 dull magenta bags.\n\
\wavy brown bags contain no other bags.\n\
\faded teal bags contain 5 muted plum bags, 1 posh indigo bag.\n\
\vibrant tomato bags contain 1 pale blue bag.\n\
\drab red bags contain 3 light teal bags, 4 shiny lime bags.\n\
\muted beige bags contain 4 light teal bags, 2 vibrant orange bags, 1 drab turquoise bag.\n\
\dotted orange bags contain 3 plaid cyan bags, 3 shiny beige bags, 1 bright chartreuse bag.\n\
\posh fuchsia bags contain 3 posh tomato bags, 5 bright blue bags.\n\
\striped lime bags contain 4 vibrant red bags, 1 bright yellow bag, 5 faded yellow bags, 3 dim gold bags.\n\
\bright white bags contain 1 clear purple bag, 2 drab crimson bags.\n\
\wavy olive bags contain 1 pale lavender bag, 2 dotted lime bags.\n\
\clear black bags contain 2 pale yellow bags.\n\
\clear brown bags contain 2 vibrant crimson bags.\n\
\dotted aqua bags contain 4 light green bags, 4 light purple bags, 3 pale blue bags.\n\
\dull red bags contain 1 bright fuchsia bag, 2 plaid crimson bags, 5 dotted salmon bags, 1 light salmon bag.\n\
\light red bags contain 2 mirrored purple bags, 2 dull teal bags.\n\
\mirrored bronze bags contain 4 striped lavender bags, 1 dotted brown bag, 1 posh maroon bag, 1 shiny chartreuse bag.\n\
\light silver bags contain 4 dim orange bags, 1 dim aqua bag, 5 plaid crimson bags.\n\
\faded violet bags contain 5 dotted tomato bags, 3 wavy beige bags.\n\
\muted violet bags contain 1 dotted brown bag.\n\
\muted black bags contain 3 shiny purple bags, 1 dark salmon bag, 3 bright lavender bags, 2 shiny white bags.\n\
\muted tomato bags contain 3 dull magenta bags, 1 muted fuchsia bag.\n\
\faded olive bags contain 3 bright coral bags, 5 dark salmon bags, 3 dotted lavender bags, 5 vibrant purple bags.\n\
\dotted indigo bags contain 2 drab yellow bags, 5 dim coral bags, 3 striped green bags.\n\
\dull turquoise bags contain 3 dark black bags.\n\
\faded fuchsia bags contain 4 dull green bags, 5 pale lavender bags.\n\
\mirrored gray bags contain 2 mirrored brown bags, 3 shiny gray bags, 2 light maroon bags.\n\
\dull violet bags contain 5 muted tomato bags.\n\
\vibrant crimson bags contain 1 vibrant tomato bag, 1 striped maroon bag, 3 drab plum bags.\n\
\pale olive bags contain 3 shiny lime bags, 4 dim turquoise bags.\n\
\muted orange bags contain 1 dull orange bag, 5 shiny tomato bags, 5 pale yellow bags, 1 faded orange bag.\n\
\vibrant orange bags contain 5 muted yellow bags, 1 plaid salmon bag, 5 dim aqua bags.\n\
\bright brown bags contain 3 muted gray bags, 2 dull cyan bags.\n\
\clear coral bags contain 3 clear crimson bags, 4 dotted yellow bags, 1 bright orange bag.\n\
\clear blue bags contain 1 posh tan bag.\n\
\shiny cyan bags contain 3 light gold bags.\n\
\plaid tomato bags contain 3 dull magenta bags, 2 clear brown bags, 1 wavy plum bag.\n\
\dim magenta bags contain 5 dark orange bags.\n\
\mirrored plum bags contain 3 dull green bags, 4 light violet bags, 2 vibrant bronze bags, 4 vibrant tomato bags.\n\
\muted green bags contain 1 dull crimson bag, 2 drab crimson bags, 2 dim red bags.\n\
\wavy gray bags contain 3 dark silver bags.\n\
\mirrored white bags contain 4 bright yellow bags, 2 bright blue bags.\n\
\dark white bags contain 2 posh turquoise bags, 2 light plum bags.\n\
\striped maroon bags contain 5 shiny white bags, 3 shiny coral bags, 3 pale salmon bags.\n\
\shiny aqua bags contain 1 bright indigo bag, 5 dim plum bags.\n\
\vibrant chartreuse bags contain 3 drab gold bags, 1 striped cyan bag, 2 drab violet bags, 1 muted black bag.\n\
\faded chartreuse bags contain 4 bright bronze bags, 3 dark silver bags, 1 bright aqua bag.\n\
\shiny olive bags contain 1 light silver bag.\n\
\dim chartreuse bags contain 5 striped tomato bags.\n\
\vibrant red bags contain 3 mirrored beige bags.\n\
\vibrant white bags contain 4 light aqua bags.\n\
\dim gold bags contain 3 plaid gray bags.\n\
\posh chartreuse bags contain 3 wavy blue bags, 5 dull maroon bags, 5 muted yellow bags.\n\
\striped purple bags contain 4 shiny orange bags, 3 dim lavender bags, 3 muted lavender bags, 5 plaid tomato bags.\n\
\plaid white bags contain 2 striped salmon bags.\n\
\striped tomato bags contain 5 bright blue bags.\n\
\bright lime bags contain 5 dark maroon bags, 1 shiny gray bag, 3 light white bags.\n\
\vibrant yellow bags contain 3 plaid magenta bags, 3 wavy chartreuse bags, 3 bright lavender bags, 1 vibrant salmon bag.\n\
\faded red bags contain 3 plaid turquoise bags, 2 wavy cyan bags, 5 dim chartreuse bags.\n\
\dim red bags contain 4 muted coral bags.\n\
\dotted beige bags contain no other bags.\n\
\pale magenta bags contain 1 light violet bag, 2 mirrored lime bags.\n\
\muted fuchsia bags contain no other bags.\n\
\vibrant purple bags contain 5 shiny purple bags, 4 dim turquoise bags, 2 bright violet bags, 1 dotted beige bag.\n\
\pale tomato bags contain 3 dull magenta bags, 5 dim turquoise bags.\n\
\wavy gold bags contain 2 muted tomato bags, 5 posh tomato bags.\n\
\dull green bags contain 1 shiny indigo bag, 3 dull maroon bags.\n\
\muted magenta bags contain 1 clear tan bag, 5 dim tomato bags, 1 plaid purple bag, 1 wavy gray bag.\n\
\drab bronze bags contain 5 dull violet bags, 1 light green bag, 3 dim turquoise bags.\n\
\vibrant coral bags contain 5 bright black bags, 5 vibrant salmon bags.\n\
\dim gray bags contain no other bags.\n\
\mirrored black bags contain 5 mirrored lavender bags.\n\
\muted cyan bags contain 5 mirrored gold bags.\n\
\light aqua bags contain 1 dull cyan bag, 4 dull lavender bags, 2 wavy crimson bags.\n\
\mirrored gold bags contain 1 light orange bag.\n\
\vibrant cyan bags contain 2 pale chartreuse bags, 5 vibrant tan bags.\n\
\dotted green bags contain 2 vibrant tan bags.\n\
\vibrant tan bags contain 5 pale lime bags.\n\
\drab magenta bags contain 3 light gold bags, 2 posh yellow bags, 4 mirrored salmon bags, 4 muted black bags.\n\
\clear teal bags contain 3 vibrant red bags, 3 dotted crimson bags, 3 plaid fuchsia bags.\n\
\dark coral bags contain 4 posh fuchsia bags, 5 mirrored fuchsia bags.\n\
\bright plum bags contain 5 vibrant blue bags.\n\
\drab indigo bags contain 2 shiny orange bags, 3 light indigo bags, 2 drab crimson bags, 5 light purple bags.\n\
\shiny chartreuse bags contain 4 clear gold bags.\n\
\plaid coral bags contain 1 shiny white bag, 2 dotted plum bags, 5 wavy indigo bags.\n\
\dull silver bags contain 3 striped white bags, 2 faded gold bags, 3 dark fuchsia bags.\n\
\dotted teal bags contain 5 clear lime bags.\n\
\posh black bags contain 4 light plum bags, 4 dark plum bags, 4 vibrant plum bags.\n\
\wavy black bags contain 5 bright bronze bags.\n\
\mirrored magenta bags contain 3 muted olive bags, 5 striped tomato bags, 2 clear lavender bags.\n\
\posh indigo bags contain 4 clear salmon bags, 3 vibrant salmon bags.\n\
\vibrant brown bags contain 2 drab turquoise bags.\n\
\pale blue bags contain 3 vibrant silver bags, 5 plaid turquoise bags.\n\
\wavy lavender bags contain 5 plaid lime bags, 1 plaid crimson bag, 5 wavy coral bags, 1 dark gold bag.\n\
\shiny tan bags contain 5 plaid blue bags, 3 shiny bronze bags, 1 mirrored gold bag, 4 faded salmon bags.\n\
\muted purple bags contain 3 muted tan bags.\n\
\mirrored crimson bags contain 2 drab plum bags, 3 muted magenta bags, 5 dim yellow bags.\n\
\muted red bags contain 1 wavy indigo bag.\n\
\dotted tomato bags contain 5 plaid turquoise bags, 1 dull lime bag.\n\
\dull orange bags contain 2 shiny yellow bags, 1 drab bronze bag, 4 shiny lime bags.\n\
\clear aqua bags contain 3 dull cyan bags, 2 bright yellow bags, 2 shiny maroon bags.\n\
\light coral bags contain 4 wavy black bags.\n\
\posh silver bags contain 5 light bronze bags, 2 clear purple bags, 5 pale fuchsia bags, 2 dull cyan bags.\n\
\drab coral bags contain 2 posh yellow bags.\n\
\light fuchsia bags contain 2 dim gray bags.\n\
\faded cyan bags contain 1 faded orange bag.\n\
\bright aqua bags contain 1 muted black bag, 1 muted chartreuse bag, 1 wavy blue bag.\n\
\drab lavender bags contain 4 posh yellow bags.\n\
\posh gray bags contain 4 shiny lime bags, 5 dotted gold bags, 4 vibrant maroon bags.\n\
\plaid blue bags contain 5 vibrant tan bags, 5 mirrored white bags, 5 shiny purple bags, 1 clear white bag.\n\
\shiny gray bags contain 2 muted fuchsia bags, 5 plaid crimson bags, 5 faded salmon bags.\n\
\light brown bags contain 4 dotted tan bags, 5 wavy coral bags, 3 plaid teal bags.\n\
\striped magenta bags contain 2 wavy turquoise bags.\n\
\striped aqua bags contain 2 light cyan bags, 2 plaid lime bags, 2 mirrored orange bags, 1 posh lime bag.\n\
\bright teal bags contain 1 faded plum bag, 1 dark blue bag, 1 posh cyan bag, 2 striped chartreuse bags.\n\
\pale green bags contain 1 muted black bag, 4 faded olive bags, 1 wavy chartreuse bag, 5 dim maroon bags.\n\
\vibrant bronze bags contain 3 vibrant lavender bags, 3 faded blue bags, 4 dim brown bags, 4 drab chartreuse bags.\n\
\pale brown bags contain 3 faded yellow bags, 1 vibrant tomato bag.\n\
\wavy aqua bags contain 1 striped magenta bag, 1 mirrored silver bag, 1 dim gold bag.\n\
\dotted yellow bags contain 2 drab turquoise bags, 2 drab maroon bags, 1 light teal bag.\n\
\pale salmon bags contain 2 bright indigo bags, 3 wavy violet bags.\n\
\posh red bags contain 3 posh blue bags, 4 muted yellow bags, 5 dark silver bags.\n\
\wavy violet bags contain 2 dotted lime bags, 2 vibrant purple bags, 1 dull maroon bag.\n\
\drab purple bags contain 4 plaid crimson bags, 1 shiny plum bag.\n\
\light black bags contain 2 faded teal bags, 1 faded yellow bag, 2 muted fuchsia bags.\n\
\dim turquoise bags contain 4 wavy beige bags, 1 shiny plum bag, 3 muted tomato bags, 4 dull magenta bags.\n\
\dark tan bags contain 5 drab chartreuse bags, 3 dim coral bags.\n\
\striped gray bags contain 5 muted red bags, 1 bright blue bag, 5 wavy maroon bags.\n\
\muted tan bags contain 5 drab turquoise bags, 1 light maroon bag, 5 plaid purple bags.\n\
\posh green bags contain 3 dotted chartreuse bags, 4 muted yellow bags, 5 shiny gray bags, 1 dark black bag.\n\
\mirrored lime bags contain 4 clear teal bags, 5 wavy coral bags, 2 mirrored beige bags.\n\
\muted plum bags contain 5 faded yellow bags, 3 posh blue bags.\n\
\dark maroon bags contain 2 dotted lavender bags.\n\
\plaid green bags contain 3 wavy yellow bags, 1 shiny indigo bag, 4 mirrored maroon bags, 5 dim gray bags.\n\
\mirrored teal bags contain 5 dim magenta bags, 1 dark magenta bag, 3 plaid green bags.\n\
\wavy crimson bags contain 1 vibrant cyan bag, 2 light beige bags.\n\
\wavy green bags contain 4 dim tan bags, 1 dotted lime bag, 5 drab white bags, 2 bright beige bags.\n\
\shiny brown bags contain 5 dark silver bags, 3 shiny maroon bags, 5 dotted lavender bags.\n\
\muted turquoise bags contain 5 dark indigo bags, 3 drab plum bags.\n\
\mirrored lavender bags contain 2 clear salmon bags, 2 plaid maroon bags, 3 wavy cyan bags.\n\
\drab blue bags contain 3 dotted olive bags, 1 shiny chartreuse bag, 1 drab magenta bag, 2 mirrored maroon bags.\n\
\bright fuchsia bags contain 1 dark olive bag.\n\
\pale turquoise bags contain 2 striped chartreuse bags.\n\
\posh white bags contain 1 striped cyan bag, 1 plaid coral bag, 2 dim gray bags, 3 shiny red bags.\n\
\drab brown bags contain 4 plaid red bags.\n\
\pale gold bags contain 2 dim tan bags, 5 dark red bags, 4 vibrant cyan bags, 5 dark violet bags.\n\
\plaid maroon bags contain 1 shiny lime bag, 4 faded lavender bags, 2 dim tan bags, 5 vibrant purple bags.\n\
\vibrant green bags contain 1 drab purple bag, 2 faded blue bags.\n\
\bright violet bags contain 1 dotted lavender bag, 5 light chartreuse bags, 4 bright indigo bags.\n\
\bright magenta bags contain 5 shiny purple bags, 5 light teal bags, 2 pale purple bags, 4 pale silver bags.\n\
\plaid orange bags contain 1 dull green bag.\n\
\posh olive bags contain 5 dark silver bags, 3 drab yellow bags, 2 dim brown bags, 4 dull gray bags.\n\
\light teal bags contain 3 dim gray bags.\n\
\shiny beige bags contain 1 dark cyan bag, 1 vibrant yellow bag.\n\
\drab cyan bags contain 4 dim silver bags, 1 wavy maroon bag.\n\
\pale gray bags contain 2 dark salmon bags.\n\
\plaid beige bags contain 3 bright purple bags, 1 drab plum bag, 4 shiny bronze bags, 3 drab salmon bags.\n\
\dull plum bags contain 4 mirrored orange bags.\n\
\shiny violet bags contain 3 drab olive bags.\n\
\striped turquoise bags contain 1 faded green bag.\n\
\dark aqua bags contain 5 clear crimson bags.\n\
\dim crimson bags contain 3 faded chartreuse bags, 3 light orange bags, 4 striped plum bags.\n\
\faded tan bags contain 1 dark cyan bag, 1 bright silver bag.\n\
\faded brown bags contain 3 faded purple bags, 1 posh orange bag, 5 light silver bags.\n\
\mirrored coral bags contain 1 vibrant tan bag.\n\
\clear olive bags contain 2 pale olive bags.\n\
\pale lime bags contain 2 muted fuchsia bags, 1 dim gray bag, 5 dotted beige bags, 5 striped teal bags.\n\
\wavy white bags contain 3 dim indigo bags, 3 plaid green bags, 2 dull gold bags.\n\
\pale plum bags contain 5 plaid gray bags.\n\
\plaid yellow bags contain 5 dark yellow bags, 1 mirrored white bag, 5 shiny white bags, 1 shiny lime bag.\n\
\drab salmon bags contain 2 vibrant blue bags, 2 plaid plum bags, 1 shiny gray bag, 3 bright black bags.\n\
\striped fuchsia bags contain 2 striped teal bags, 5 drab black bags, 2 bright magenta bags.\n\
\mirrored fuchsia bags contain 3 drab black bags.\n\
\dark indigo bags contain 1 dull lime bag, 4 vibrant cyan bags, 1 bright gray bag, 3 pale purple bags.\n\
\posh blue bags contain 1 shiny yellow bag.\n\
\dark turquoise bags contain 4 bright yellow bags, 3 wavy bronze bags.\n\
\posh tan bags contain 4 shiny plum bags, 4 dark silver bags, 3 wavy beige bags, 1 mirrored fuchsia bag.\n\
\bright turquoise bags contain 1 dotted green bag, 4 faded silver bags, 1 plaid turquoise bag.\n\
\vibrant teal bags contain 4 plaid teal bags, 3 pale blue bags.\n\
\plaid purple bags contain 2 dim tan bags, 5 light turquoise bags.\n\
\drab plum bags contain 2 dull green bags, 5 posh lavender bags, 3 drab tan bags.\n\
\dark blue bags contain 3 mirrored indigo bags, 3 dark red bags.\n\
\vibrant black bags contain 2 shiny red bags, 5 posh brown bags, 1 mirrored yellow bag.\n\
\mirrored brown bags contain 1 shiny indigo bag, 2 shiny yellow bags, 5 shiny blue bags, 5 clear yellow bags.\n\
\dotted crimson bags contain 3 posh tomato bags, 4 vibrant blue bags, 5 dark violet bags, 5 posh violet bags.\n\
\shiny bronze bags contain 1 plaid maroon bag.\n\
\drab orange bags contain 4 dotted crimson bags, 4 clear bronze bags, 3 pale turquoise bags, 5 dim coral bags.\n\
\wavy fuchsia bags contain 4 posh fuchsia bags, 5 wavy purple bags, 2 muted cyan bags.\n\
\plaid black bags contain 2 drab crimson bags, 4 vibrant tomato bags, 4 clear bronze bags, 4 faded lavender bags.\n\
\mirrored red bags contain 3 shiny crimson bags.\n\
\faded tomato bags contain 1 clear aqua bag, 3 vibrant bronze bags.\n\
\light turquoise bags contain 2 posh silver bags, 2 shiny black bags, 5 muted blue bags.\n\
\faded crimson bags contain 5 posh blue bags, 4 dim maroon bags.\n\
\dull magenta bags contain no other bags.\n\
\dark plum bags contain 5 vibrant purple bags, 5 wavy brown bags, 2 dark indigo bags.\n\
\dotted white bags contain 1 dark teal bag, 5 dull white bags, 2 pale salmon bags, 1 dotted plum bag.\n\
\wavy indigo bags contain 5 shiny maroon bags, 4 shiny lime bags.\n\
\faded coral bags contain 5 dim indigo bags, 1 striped brown bag.\n\
\vibrant turquoise bags contain 4 dotted magenta bags, 5 bright lavender bags, 5 plaid turquoise bags, 5 striped lavender bags.\n\
\muted gray bags contain 1 mirrored white bag.\n\
\dotted gold bags contain 4 dark brown bags.\n\
\dim salmon bags contain 3 light gold bags, 4 plaid purple bags.\n\
\clear salmon bags contain 3 striped lavender bags, 5 posh teal bags, 4 vibrant turquoise bags.\n\
\vibrant olive bags contain 3 striped fuchsia bags, 3 muted indigo bags, 1 dotted yellow bag, 4 bright white bags.\n\
\bright salmon bags contain 1 drab orange bag.\n\
\muted maroon bags contain 5 dim crimson bags, 4 dark red bags, 5 plaid purple bags.\n\
\light tan bags contain 2 dotted blue bags.\n\
\dotted turquoise bags contain 4 faded lime bags, 2 dotted silver bags, 4 mirrored cyan bags.\n\
\shiny magenta bags contain 1 mirrored salmon bag, 3 dim fuchsia bags, 3 muted red bags.\n\
\dark cyan bags contain 5 dark brown bags, 5 vibrant silver bags, 5 dim green bags.\n\
\dim violet bags contain 4 vibrant turquoise bags, 1 pale crimson bag, 4 bright red bags, 3 muted black bags.\n\
\dull aqua bags contain 3 drab teal bags.\n\
\dim aqua bags contain 3 vibrant blue bags, 4 pale salmon bags, 5 shiny bronze bags, 2 dim olive bags.\n\
\light gray bags contain 4 dull white bags, 4 dim blue bags.\n\
\dull salmon bags contain 1 striped fuchsia bag, 5 dark white bags, 2 pale tan bags.\n\
\plaid teal bags contain 1 dim olive bag, 2 faded purple bags, 4 shiny gray bags, 1 shiny salmon bag.\n\
\clear tan bags contain 1 clear lime bag, 4 mirrored turquoise bags, 4 bright white bags, 3 drab bronze bags.\n\
\wavy beige bags contain 2 muted fuchsia bags, 3 clear bronze bags, 2 bright yellow bags.\n\
\plaid salmon bags contain 2 drab bronze bags, 2 drab chartreuse bags, 4 plaid crimson bags, 5 shiny lime bags.\n\
\drab lime bags contain 2 clear orange bags, 4 dotted silver bags, 5 dark maroon bags, 5 dim magenta bags.\n\
\bright crimson bags contain 5 shiny maroon bags.\n\
\clear turquoise bags contain 4 vibrant blue bags.\n\
\clear lavender bags contain 3 pale fuchsia bags, 2 bright lavender bags, 2 dark orange bags, 1 dull magenta bag.\n\
\clear green bags contain 3 clear orange bags.\n\
\pale cyan bags contain 2 dull purple bags, 1 clear olive bag.\n\
\posh lime bags contain 2 clear purple bags.\n\
\dim lavender bags contain 3 clear crimson bags, 4 striped violet bags.\n\
\faded indigo bags contain 5 dull cyan bags, 2 plaid blue bags, 4 striped teal bags.\n\
\posh aqua bags contain 5 bright aqua bags, 2 shiny indigo bags, 2 drab silver bags, 1 vibrant turquoise bag.\n\
\dark magenta bags contain 3 light silver bags.\n\
\clear cyan bags contain 3 pale fuchsia bags, 2 shiny coral bags, 1 dull magenta bag, 4 dim turquoise bags.\n\
\pale yellow bags contain 1 vibrant tan bag, 4 drab plum bags.\n\
\dim plum bags contain 4 striped gray bags, 2 posh tomato bags, 1 drab black bag.\n\
\dark purple bags contain 3 bright indigo bags, 3 dull purple bags.\n\
\clear plum bags contain 4 bright cyan bags, 5 dim turquoise bags.\n\
\faded magenta bags contain 1 muted olive bag, 3 plaid chartreuse bags.\n\
\mirrored aqua bags contain 2 clear lavender bags.\n\
\light white bags contain 3 dim gray bags, 3 bright white bags, 2 striped teal bags, 4 light green bags.\n\
\clear gold bags contain 2 wavy coral bags.\n\
\dull lavender bags contain 2 wavy beige bags, 3 drab black bags.\n\
\dark yellow bags contain 3 wavy gray bags, 1 dark salmon bag, 2 vibrant tan bags.\n\
\dotted black bags contain 4 dim plum bags, 2 bright white bags.\n\
\posh tomato bags contain 5 wavy beige bags, 1 dim gray bag, 3 shiny lime bags, 5 bright black bags.\n\
\bright indigo bags contain 4 striped teal bags, 1 shiny yellow bag, 5 dotted beige bags.\n\
\dim purple bags contain 5 dark tan bags, 5 striped lavender bags.\n\
\posh plum bags contain 5 posh maroon bags.\n\
\pale bronze bags contain 5 shiny silver bags.\n\
\striped crimson bags contain 2 dark indigo bags.\n\
\plaid fuchsia bags contain 1 shiny lime bag, 4 mirrored orange bags.\n\
\dark orange bags contain 2 dotted lavender bags, 1 posh blue bag, 2 muted yellow bags.\n\
\dim blue bags contain 2 clear yellow bags.\n\
\clear violet bags contain 2 shiny bronze bags.\n\
\shiny blue bags contain 4 dark salmon bags, 5 posh plum bags.\n\
\dull lime bags contain 5 plaid green bags, 3 bright black bags, 4 drab maroon bags.\n\
\muted lavender bags contain 1 dark gray bag, 2 bright crimson bags.\n\
\plaid gray bags contain 5 bright black bags, 5 pale silver bags.\n\
\light yellow bags contain 2 mirrored crimson bags.\n\
\muted chartreuse bags contain 1 pale fuchsia bag, 1 plaid purple bag.\n\
\dim lime bags contain 3 dull maroon bags, 3 bright red bags, 3 drab black bags.\n\
\light purple bags contain 5 dull violet bags, 5 vibrant lavender bags.\n\
\posh orange bags contain 5 pale salmon bags, 5 light chartreuse bags, 1 dark red bag.\n\
\vibrant violet bags contain 3 shiny gray bags.\n\
\pale silver bags contain 5 mirrored salmon bags.\n\
\light tomato bags contain 2 dim tan bags, 1 faded silver bag, 4 plaid yellow bags, 3 drab gold bags.\n\
\posh salmon bags contain 3 bright plum bags.\n\
\dotted plum bags contain 4 pale gold bags, 1 vibrant lavender bag, 2 pale blue bags.\n\
\shiny lavender bags contain 1 dull gray bag, 4 muted teal bags, 2 dotted yellow bags, 2 bright turquoise bags.\n\
\dim beige bags contain 2 wavy yellow bags, 2 drab violet bags, 1 wavy gold bag, 5 clear cyan bags.\n\
\posh cyan bags contain 2 clear lavender bags, 4 clear salmon bags, 4 posh tan bags.\n\
\light maroon bags contain 5 dark violet bags, 3 plaid lime bags, 3 wavy gold bags, 5 dotted lime bags.\n\
\dotted lime bags contain 3 clear bronze bags, 5 dim turquoise bags.\n\
\dim bronze bags contain 3 light fuchsia bags.\n\
\faded black bags contain 3 light bronze bags, 2 bright beige bags.\n\
\striped white bags contain 1 mirrored white bag.\n\
\clear red bags contain 1 pale orange bag, 2 clear orange bags, 1 dull maroon bag, 3 dark bronze bags.\n\
\muted lime bags contain 5 dim fuchsia bags.\n\
\vibrant gold bags contain 4 wavy blue bags, 3 clear gray bags, 5 vibrant turquoise bags.\n\
\clear silver bags contain 3 light violet bags, 5 dull turquoise bags.\n\
\muted indigo bags contain 3 dark orange bags, 4 light gold bags.\n\
\bright green bags contain 4 dark red bags.\n\
\vibrant plum bags contain 5 dull tomato bags, 4 bright cyan bags, 4 plaid purple bags.\n\
\wavy cyan bags contain 4 muted plum bags.\n\
\plaid cyan bags contain 4 clear orange bags, 3 dull chartreuse bags.\n\
\faded salmon bags contain 1 dim gray bag.\n\
\shiny turquoise bags contain 3 light aqua bags, 1 vibrant beige bag, 4 clear turquoise bags, 1 faded silver bag.\n\
\dotted silver bags contain 5 dotted blue bags, 1 dotted salmon bag.\n\
\striped black bags contain 2 plaid crimson bags, 5 dotted silver bags, 5 dull maroon bags.\n\
\dull indigo bags contain 4 dim magenta bags, 4 shiny magenta bags, 2 pale yellow bags, 1 plaid cyan bag.\n\
\bright coral bags contain 1 shiny coral bag, 2 muted tomato bags, 4 drab crimson bags.\n\
\faded bronze bags contain 5 posh cyan bags, 5 light beige bags, 1 drab yellow bag, 3 striped lime bags.\n\
\drab green bags contain 1 clear cyan bag.\n\
\pale black bags contain 2 muted yellow bags.\n\
\muted salmon bags contain 4 plaid crimson bags, 2 vibrant lime bags.\n\
\dark violet bags contain 4 clear cyan bags, 1 mirrored maroon bag, 5 drab black bags.\n\
\faded aqua bags contain 3 pale fuchsia bags, 1 clear teal bag, 5 light white bags.\n\
\vibrant indigo bags contain 5 light turquoise bags.\n\
\bright blue bags contain 1 shiny purple bag, 1 wavy brown bag.\n\
\dark olive bags contain 2 wavy beige bags.\n\
\vibrant beige bags contain 3 wavy beige bags, 1 posh violet bag, 4 dark teal bags.\n\
\dotted olive bags contain 5 dotted magenta bags.\n\
\muted coral bags contain 2 vibrant silver bags, 3 dim coral bags.\n\
\dark chartreuse bags contain 4 faded tomato bags, 2 light violet bags, 4 clear silver bags.\n\
\light green bags contain no other bags.\n\
\bright gray bags contain 2 drab yellow bags, 2 muted indigo bags.\n\
\posh crimson bags contain 5 plaid indigo bags, 3 wavy lime bags, 2 dull turquoise bags, 5 faded salmon bags.\n\
\drab yellow bags contain 3 shiny purple bags, 4 dark salmon bags, 3 shiny coral bags, 2 shiny plum bags.\n\
\dull white bags contain 3 plaid lavender bags, 4 dark beige bags, 1 posh beige bag.\n\
\drab olive bags contain 1 shiny cyan bag, 3 bright fuchsia bags.\n\
\striped olive bags contain 2 drab aqua bags, 3 muted chartreuse bags, 2 clear red bags, 1 posh turquoise bag.\n\
\posh purple bags contain 1 dull lime bag, 2 dull plum bags, 3 dark black bags.\n\
\mirrored green bags contain 2 mirrored salmon bags.\n\
\dull teal bags contain 1 dark gray bag, 1 posh indigo bag, 5 light coral bags.\n\
\dim indigo bags contain 5 dark maroon bags.\n\
\faded gray bags contain 4 muted crimson bags.\n\
\light blue bags contain 2 plaid violet bags, 3 dim tomato bags, 4 plaid crimson bags, 5 dull orange bags.\n\
\plaid turquoise bags contain 2 muted blue bags, 5 clear white bags, 1 dull violet bag.\n\
\striped gold bags contain 2 striped brown bags, 1 wavy silver bag.\n\
\faded blue bags contain 1 dark lavender bag, 4 muted yellow bags, 2 bright yellow bags, 2 dim tan bags.\n\
\clear beige bags contain 5 dim tan bags, 4 faded blue bags, 2 bright green bags.\n\
\shiny indigo bags contain 1 shiny purple bag.\n\
\striped brown bags contain 2 light gold bags.\n\
\shiny black bags contain 1 posh red bag, 5 bright black bags, 3 posh plum bags, 5 clear white bags.\n\
\pale aqua bags contain 5 plaid purple bags, 3 wavy orange bags, 5 dotted purple bags, 4 bright lime bags.\n\
\dark beige bags contain 3 clear white bags, 3 wavy yellow bags, 3 posh turquoise bags, 2 posh tan bags.\n\
\striped green bags contain 3 posh red bags, 2 shiny salmon bags, 4 mirrored salmon bags, 3 posh teal bags.\n\
\pale violet bags contain 5 wavy turquoise bags, 1 bright aqua bag.\n\
\dim tomato bags contain 5 dim tan bags.\n\
\mirrored cyan bags contain 5 shiny indigo bags, 3 faded blue bags.\n\
\plaid magenta bags contain 3 mirrored brown bags, 5 pale olive bags, 2 clear purple bags.\n\
\dotted tan bags contain 5 plaid turquoise bags.\n\
\dull fuchsia bags contain 2 shiny crimson bags, 3 clear olive bags.\n\
\clear orange bags contain 5 dotted lime bags, 2 dark olive bags, 3 bright yellow bags, 2 wavy violet bags.\n\
\light indigo bags contain 1 drab plum bag, 5 dim lime bags.\n\
\muted aqua bags contain 5 faded red bags, 5 pale blue bags, 5 dull lavender bags.\n\
\faded plum bags contain 5 drab turquoise bags.\n\
\dark gold bags contain 4 faded salmon bags, 4 bright black bags, 4 dim chartreuse bags.\n\
\striped bronze bags contain 2 dark beige bags.\n\
\mirrored purple bags contain 3 dim turquoise bags, 4 shiny bronze bags.\n\
\faded white bags contain 4 faded tomato bags, 3 faded aqua bags, 5 faded gold bags, 4 vibrant orange bags.\n\
\wavy turquoise bags contain 2 dotted olive bags, 1 muted yellow bag.\n\
\striped blue bags contain 5 wavy bronze bags, 3 shiny chartreuse bags.\n\
\dull gray bags contain 4 pale lavender bags.\n\
\plaid plum bags contain 4 dim olive bags, 3 faded olive bags.\n\
\pale coral bags contain 1 faded yellow bag, 3 shiny plum bags, 4 striped tomato bags, 4 shiny bronze bags.\n\
\drab violet bags contain 2 plaid black bags, 5 bright violet bags, 3 pale blue bags.\n\
\vibrant gray bags contain 4 vibrant indigo bags, 1 dull beige bag, 1 dull plum bag, 1 dotted brown bag.\n\
\drab gray bags contain 5 mirrored brown bags, 5 bright coral bags.\n\
\pale chartreuse bags contain 2 dull cyan bags, 4 mirrored fuchsia bags.\n\
\dotted lavender bags contain 2 bright blue bags, 3 light chartreuse bags.\n\
\dull tomato bags contain 5 wavy yellow bags.\n\
\dull purple bags contain 5 wavy brown bags, 1 muted yellow bag.\n\
\wavy orange bags contain 4 bright orange bags, 1 dotted purple bag, 3 dull blue bags, 3 light crimson bags.\n\
\dotted purple bags contain 1 bright cyan bag, 5 wavy violet bags, 3 vibrant cyan bags.\n\
\light lavender bags contain 4 shiny salmon bags, 4 mirrored salmon bags, 2 wavy magenta bags.\n\
\plaid crimson bags contain 3 dotted salmon bags, 2 plaid maroon bags, 5 pale chartreuse bags, 3 dotted beige bags.\n\
\plaid lavender bags contain 2 clear bronze bags, 5 light silver bags, 1 bright lavender bag.\n\
\wavy lime bags contain 1 dim coral bag.\n\
\clear tomato bags contain 1 dark violet bag.\n\
\light gold bags contain 1 striped teal bag, 2 vibrant purple bags, 5 dark salmon bags.\n\
\faded turquoise bags contain 1 shiny lime bag, 1 posh orange bag, 1 plaid maroon bag.\n\
\wavy tomato bags contain 2 plaid plum bags, 1 dull lime bag, 3 dull maroon bags.\n\
\faded beige bags contain 3 clear maroon bags, 1 dotted brown bag.\n\
\dark black bags contain 1 faded salmon bag, 1 light green bag, 4 clear bronze bags, 4 dotted lavender bags.\n\
\striped violet bags contain 4 mirrored beige bags.\n\
\plaid chartreuse bags contain 2 dotted purple bags.\n\
\posh maroon bags contain 5 wavy yellow bags, 2 pale chartreuse bags.\n\
\posh yellow bags contain 5 mirrored blue bags, 2 wavy beige bags.\n\
\bright black bags contain 3 clear bronze bags, 5 wavy gray bags.\n\
\shiny tomato bags contain 3 dotted plum bags, 1 shiny orange bag, 4 faded teal bags, 1 bright lavender bag.\n\
\dotted chartreuse bags contain 2 muted yellow bags, 4 faded cyan bags, 3 striped indigo bags, 1 drab red bag.\n\
\pale tan bags contain 3 dotted chartreuse bags.\n\
\dim white bags contain 2 dim tomato bags, 3 striped indigo bags, 1 faded red bag, 3 mirrored turquoise bags.\n\
\mirrored salmon bags contain 1 pale chartreuse bag, 3 faded salmon bags, 2 dark silver bags.\n\
\vibrant fuchsia bags contain 3 dim olive bags, 1 striped magenta bag, 2 dark magenta bags, 2 drab white bags.\n\
\drab silver bags contain 2 dotted silver bags, 4 wavy yellow bags, 2 dim turquoise bags, 2 shiny maroon bags.\n\
\dull yellow bags contain 2 posh teal bags, 1 muted chartreuse bag, 1 pale salmon bag, 1 posh beige bag.\n\
\clear fuchsia bags contain 4 light turquoise bags, 1 dull white bag, 3 plaid indigo bags, 1 shiny gold bag.\n\
\dark bronze bags contain 2 faded salmon bags.\n\
\vibrant lime bags contain 3 dull chartreuse bags, 5 shiny yellow bags.\n\
\wavy tan bags contain 4 wavy indigo bags, 3 dull crimson bags.\n\
\mirrored tan bags contain 4 dim salmon bags, 5 dim tomato bags, 4 mirrored salmon bags, 3 drab indigo bags.\n\
\plaid bronze bags contain 2 dotted silver bags, 1 striped black bag, 2 dotted tomato bags.\n\
\shiny silver bags contain 2 muted salmon bags, 5 bright magenta bags, 2 drab lime bags.\n\
\mirrored violet bags contain 1 shiny yellow bag, 5 vibrant tan bags.\n\
\dotted red bags contain 3 posh beige bags, 3 posh lavender bags, 3 shiny lavender bags, 4 pale bronze bags.\n\
\mirrored chartreuse bags contain 3 dark coral bags, 1 muted cyan bag.\n\
\drab crimson bags contain 4 bright blue bags, 4 faded salmon bags.\n\
\striped salmon bags contain 4 posh fuchsia bags, 4 faded salmon bags, 3 shiny yellow bags.\n\
\dark gray bags contain 3 bright bronze bags.\n\
\striped tan bags contain 3 vibrant green bags.\n\
\dull maroon bags contain 1 muted tomato bag.\n\
\light crimson bags contain 1 posh green bag.\n\
\faded lavender bags contain 2 dotted beige bags.\n\
\clear white bags contain 2 bright bronze bags, 2 wavy gray bags, 4 dull magenta bags, 2 shiny lime bags.\n\
\dim orange bags contain 5 drab gray bags, 4 drab tan bags, 3 faded black bags, 2 pale cyan bags.\n\
\faded green bags contain 4 dim blue bags.\n\
\plaid aqua bags contain 2 drab indigo bags, 5 pale turquoise bags, 1 pale tomato bag.\n\
\dotted magenta bags contain 5 pale lavender bags, 2 shiny yellow bags, 3 posh tomato bags.\n\
\faded maroon bags contain 5 wavy plum bags, 2 shiny lime bags.\n\
\dim silver bags contain 2 dull cyan bags, 1 shiny salmon bag, 1 striped salmon bag.\n\
\dotted salmon bags contain 1 posh tomato bag, 4 bright black bags.\n\
\vibrant salmon bags contain 2 dark silver bags.\n\
\striped beige bags contain 2 clear tomato bags, 1 mirrored indigo bag, 2 pale chartreuse bags, 3 dull yellow bags.\n\
\bright orange bags contain 1 pale lime bag.\n\
\muted crimson bags contain 1 dotted lavender bag.\n\
\muted bronze bags contain 3 light green bags.\n\
\shiny fuchsia bags contain 4 plaid indigo bags, 5 bright fuchsia bags.\n\
\bright red bags contain 2 posh fuchsia bags, 2 clear cyan bags.\n\
\dim maroon bags contain 1 dotted beige bag, 2 drab black bags, 5 posh tomato bags, 3 striped lavender bags.\n\
\bright gold bags contain 5 dim chartreuse bags, 1 vibrant salmon bag, 2 faded black bags.\n\
\drab white bags contain 1 clear yellow bag, 4 pale cyan bags, 5 dark yellow bags.\n\
\clear purple bags contain 2 wavy brown bags, 3 dull magenta bags, 5 dim gray bags, 4 shiny purple bags.\n\
\dim green bags contain 2 posh turquoise bags, 4 light bronze bags.\n\
\dull blue bags contain 1 light salmon bag, 1 posh red bag, 2 clear brown bags.\n\
\bright silver bags contain 3 dotted bronze bags, 1 plaid blue bag, 2 light coral bags.\n\
\bright bronze bags contain 3 dull magenta bags, 3 light white bags.\n\
\shiny gold bags contain 3 wavy gold bags.\n\
\wavy bronze bags contain 4 dull lime bags, 2 shiny white bags, 2 dim silver bags.\n\
\muted yellow bags contain 5 dark bronze bags, 3 light white bags, 5 shiny coral bags, 4 shiny lime bags.\n\
\faded orange bags contain 3 plaid maroon bags, 5 shiny plum bags, 2 mirrored white bags.\n\
\vibrant silver bags contain 1 bright coral bag, 5 wavy blue bags, 1 mirrored maroon bag.\n\
\mirrored maroon bags contain 5 drab crimson bags, 4 striped tomato bags.\n\
\striped silver bags contain 2 dotted olive bags, 3 muted white bags, 5 dark black bags, 5 light gray bags.\n\
\drab gold bags contain 2 clear coral bags.\n\
\plaid tan bags contain 2 plaid orange bags, 2 muted beige bags, 2 mirrored purple bags.\n\
\bright beige bags contain 5 light plum bags.\n\
\light salmon bags contain 1 dark black bag, 5 mirrored salmon bags, 1 dim silver bag.\n\
\dull beige bags contain 2 dark lavender bags.\n\
\posh magenta bags contain 3 muted yellow bags.\n\
\dotted violet bags contain 1 wavy brown bag, 3 dim turquoise bags.\n\
\vibrant blue bags contain 4 wavy gray bags, 2 light turquoise bags, 1 drab bronze bag, 4 wavy cyan bags.\n\
\wavy purple bags contain 3 dotted olive bags, 2 dull lime bags.\n\
\wavy plum bags contain 3 posh chartreuse bags.\n\
\dark silver bags contain no other bags.\n\
\dark teal bags contain 4 posh red bags, 4 dark lavender bags.\n\
\plaid lime bags contain 2 muted fuchsia bags.\n\
\posh violet bags contain 5 clear olive bags.\n\
\plaid olive bags contain 4 vibrant tan bags, 5 plaid fuchsia bags.\n\
\mirrored indigo bags contain 5 posh maroon bags, 5 light turquoise bags.\n\
\vibrant maroon bags contain 4 vibrant beige bags.\n\
\light magenta bags contain 2 vibrant gray bags, 4 clear crimson bags.\n\
\clear indigo bags contain 4 light gray bags, 1 plaid blue bag.\n\
\shiny red bags contain 4 vibrant blue bags, 1 wavy brown bag, 5 striped maroon bags, 5 posh orange bags.\n\
\dark crimson bags contain 2 bright yellow bags.\n\
\dull crimson bags contain 5 striped salmon bags, 3 faded purple bags, 4 clear tomato bags.\n\
\pale red bags contain 1 dotted cyan bag, 2 dark turquoise bags.\n\
\striped plum bags contain 4 faded yellow bags, 1 posh blue bag.\n\
\dim olive bags contain 1 light green bag, 1 faded salmon bag.\n\
\dim brown bags contain 5 drab bronze bags, 2 dark red bags, 3 shiny gold bags.\n\
\pale orange bags contain 3 pale gray bags, 1 clear olive bag.\n\
\shiny green bags contain 2 clear turquoise bags, 1 mirrored aqua bag, 2 dull red bags, 4 dull bronze bags.\n\
\dotted fuchsia bags contain 3 vibrant bronze bags, 1 dull maroon bag, 5 wavy indigo bags.\n\
\posh teal bags contain 4 pale blue bags, 1 pale lime bag.\n\
\pale teal bags contain 5 muted beige bags, 5 dark salmon bags, 2 faded black bags.\n\
\vibrant aqua bags contain 5 vibrant silver bags, 5 shiny maroon bags.\n\
\light olive bags contain 2 plaid turquoise bags.\n\
\dim teal bags contain 1 muted magenta bag, 5 wavy maroon bags, 5 striped brown bags, 4 clear plum bags.\n\
\faded silver bags contain 2 dull lavender bags.\n\
\dark fuchsia bags contain 2 dim black bags, 3 dotted olive bags.\n\
\dotted cyan bags contain 3 clear gold bags.\n\
\pale beige bags contain 2 clear turquoise bags.\n\
\vibrant magenta bags contain 5 light bronze bags, 3 drab black bags.\n\
\pale indigo bags contain 2 vibrant cyan bags.\n\
\clear bronze bags contain 1 vibrant salmon bag.\n\
\pale white bags contain 4 faded orange bags.\n\
\light chartreuse bags contain no other bags.\n\
\striped chartreuse bags contain 4 clear gray bags.\n\
\dark salmon bags contain 5 wavy gray bags, 1 light chartreuse bag, 2 clear gray bags.\n\
\muted gold bags contain 3 bright lavender bags, 4 vibrant yellow bags, 3 posh silver bags.\n\
\muted white bags contain 4 muted blue bags, 1 light gold bag.\n\
\dim tan bags contain 5 posh fuchsia bags, 5 wavy brown bags, 1 dim turquoise bag.\n\
\dull black bags contain 5 dotted cyan bags, 2 posh turquoise bags, 5 pale salmon bags.\n\
\faded lime bags contain 4 mirrored orange bags, 2 vibrant orange bags, 2 dull lavender bags, 3 dotted chartreuse bags.\n\
\drab tan bags contain 5 shiny maroon bags, 4 mirrored white bags, 3 dull plum bags, 4 shiny plum bags.\n\
\muted olive bags contain 4 faded blue bags.\n\
\dull chartreuse bags contain 4 shiny cyan bags, 5 dark silver bags, 2 dark violet bags, 2 bright lavender bags.\n\
\posh turquoise bags contain 4 pale orange bags, 2 dotted yellow bags, 3 bright tomato bags, 2 vibrant turquoise bags.\n\
\dull tan bags contain 2 mirrored purple bags.\n\
\drab teal bags contain 1 dull lavender bag.\n\
\light beige bags contain 3 plaid yellow bags, 4 faded lavender bags.\n\
\wavy blue bags contain 3 shiny purple bags.\n\
\striped orange bags contain 4 vibrant cyan bags.\n\
\shiny orange bags contain 2 shiny black bags, 2 shiny coral bags.\n\
\dull brown bags contain 3 wavy bronze bags.\n\
\mirrored silver bags contain 4 vibrant tan bags, 4 faded fuchsia bags, 5 dark red bags, 3 wavy blue bags.\n\
\wavy teal bags contain 1 pale coral bag.\n\
\drab black bags contain 4 wavy brown bags, 4 striped teal bags, 2 muted fuchsia bags, 4 shiny indigo bags.\n\
\drab aqua bags contain 2 bright orange bags, 1 vibrant tan bag.\n\
\mirrored orange bags contain 2 striped salmon bags, 1 wavy gray bag, 5 pale blue bags, 4 mirrored salmon bags.\n\
\muted teal bags contain 4 drab tan bags, 4 posh beige bags, 4 faded green bags.\n\
\faded yellow bags contain 3 shiny purple bags, 3 wavy blue bags, 1 muted fuchsia bag.\n\
\shiny maroon bags contain 3 striped salmon bags, 5 wavy violet bags.\n\
\wavy silver bags contain 4 faded blue bags, 2 vibrant salmon bags, 5 plaid gray bags.\n\
\shiny white bags contain 4 dark salmon bags.\n\
\posh brown bags contain 5 drab red bags.\n\
\bright purple bags contain 2 dim silver bags.\n\
\dotted brown bags contain 1 dotted yellow bag.\n\
\striped lavender bags contain 5 shiny gold bags, 5 mirrored fuchsia bags.\n\
\dark lavender bags contain 3 bright violet bags, 5 dull violet bags, 1 shiny coral bag.\n\
\drab maroon bags contain 2 light white bags, 2 vibrant turquoise bags, 2 vibrant cyan bags.\n\
\pale maroon bags contain 3 dim red bags.\n\
\plaid gold bags contain 4 posh bronze bags.\n\
\striped coral bags contain 5 drab blue bags, 3 muted bronze bags.\n\
\drab fuchsia bags contain 1 shiny bronze bag.\n\
\light cyan bags contain 1 dotted fuchsia bag, 1 drab violet bag.\n\
\plaid silver bags contain 3 clear gray bags, 5 light teal bags, 3 vibrant tan bags, 1 bright white bag.\n\
\wavy coral bags contain 1 shiny black bag, 3 mirrored maroon bags.\n\
\posh gold bags contain 3 bright yellow bags, 2 dark lavender bags.\n\
\drab beige bags contain 5 bright gray bags, 3 pale olive bags, 3 plaid chartreuse bags.\n\
\wavy salmon bags contain 5 shiny beige bags, 1 posh violet bag.\n\
\pale fuchsia bags contain 4 wavy gray bags, 1 wavy beige bag.\n\
\dim fuchsia bags contain 1 dim beige bag, 4 muted cyan bags, 1 clear lavender bag.\n\
\bright tan bags contain 2 drab crimson bags.\n\
\striped red bags contain 2 muted black bags.\n\
\dark green bags contain 5 plaid red bags.\n\
\faded gold bags contain 2 muted white bags, 5 clear yellow bags, 1 light fuchsia bag, 4 dotted violet bags.\n\
\striped cyan bags contain 2 drab purple bags, 1 shiny fuchsia bag, 2 light salmon bags.\n\
\dull bronze bags contain 1 faded black bag, 4 dull lavender bags.\n\
\dull cyan bags contain 5 faded salmon bags, 1 clear bronze bag.\n\
\shiny plum bags contain 2 dark silver bags.\n\
\mirrored beige bags contain 3 posh red bags, 1 clear chartreuse bag, 2 muted magenta bags.\n\
\mirrored tomato bags contain 3 posh lavender bags, 4 light violet bags, 1 wavy crimson bag, 1 dim tan bag.\n\
\light plum bags contain 5 dim tan bags, 2 dotted beige bags, 4 posh blue bags, 4 light green bags.\n\
\mirrored blue bags contain 4 faded lavender bags, 4 pale salmon bags.\n\
\clear maroon bags contain 3 dark salmon bags, 2 shiny brown bags, 4 wavy purple bags, 1 plaid yellow bag.\n\
\bright maroon bags contain 4 pale turquoise bags, 4 drab violet bags, 2 clear lime bags.\n\
\plaid red bags contain 1 drab turquoise bag, 1 striped teal bag, 4 plaid yellow bags.\n\
\mirrored turquoise bags contain 3 clear orange bags, 5 bright blue bags, 3 posh plum bags.\n\
\dark brown bags contain 5 striped fuchsia bags, 5 mirrored plum bags, 1 wavy crimson bag, 4 clear gray bags.\n\
\muted silver bags contain 2 dim tan bags, 2 muted crimson bags.\n\
\dark red bags contain 2 wavy beige bags, 1 clear bronze bag, 5 shiny coral bags, 3 shiny indigo bags.\n\
\dark tomato bags contain 2 faded gray bags, 2 dim tomato bags.\n\
\muted blue bags contain 5 posh silver bags.\n\
\wavy magenta bags contain 4 faded tomato bags.\n\
\plaid violet bags contain 5 dim aqua bags, 4 drab orange bags, 4 posh lavender bags, 2 dull yellow bags.\n\
\faded purple bags contain 4 vibrant bronze bags, 5 bright bronze bags, 1 faded red bag, 4 clear tan bags.\n\
\wavy chartreuse bags contain 5 bright white bags, 2 shiny coral bags, 3 bright cyan bags.\n\
\plaid indigo bags contain 3 pale purple bags, 2 posh orange bags.\n\
\bright tomato bags contain 5 plaid blue bags, 2 posh orange bags, 1 vibrant purple bag.\n\
\wavy maroon bags contain 1 striped white bag.\n\
\bright lavender bags contain 3 faded salmon bags, 5 mirrored maroon bags, 5 posh tomato bags.\n\
\light orange bags contain 2 bright blue bags, 1 dark yellow bag, 2 clear gray bags, 2 striped salmon bags.\n\
\posh beige bags contain 2 dark orange bags.\n\
\clear chartreuse bags contain 5 striped maroon bags, 5 light chartreuse bags, 4 drab black bags.\n\
\dotted blue bags contain 4 clear cyan bags.\n\
\dotted coral bags contain 4 shiny tomato bags, 1 posh gold bag, 5 muted olive bags.\n\
\striped yellow bags contain 5 faded purple bags, 4 drab purple bags, 5 dark yellow bags, 5 muted red bags.\n\
\dull gold bags contain 1 light turquoise bag.\n\
\dotted maroon bags contain 3 dark maroon bags, 2 faded yellow bags.\n\
\clear yellow bags contain 2 plaid maroon bags, 5 bright gray bags, 4 pale olive bags, 5 shiny indigo bags.\n\
\clear crimson bags contain 5 faded yellow bags.\n\
\dotted bronze bags contain 2 muted tomato bags."
