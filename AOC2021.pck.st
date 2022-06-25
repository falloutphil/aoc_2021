'From Cuis 6.0 [latest update: #5069] on 26 June 2022 at 12:20:47 am'!
'Description '!
!provides: 'AOC2021' 1 2!
SystemOrganization addCategory: 'AOC2021'!


!classDefinition: #Day01 category: 'AOC2021'!
Object subclass: #Day01
	instanceVariableNames: 'depths count'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AOC2021'!
!classDefinition: 'Day01 class' category: 'AOC2021'!
Day01 class
	instanceVariableNames: ''!


!Day01 methodsFor: 'evaluating' stamp: 'pab 6/26/2022 00:16:59'!
calculatePart1
	"Return result for Part 1"
	count := 0..
	2 to: (depths size) do: [ :i | (depths at: i) > (depths at: (i-1))
		ifTrue: [ count := count + 1 ] ].
	^ count
	! !

!Day01 methodsFor: 'evaluating' stamp: 'pab 6/26/2022 00:19:43'!
doIt: filenameString
	"Hint: Day01 new doIt: '/home/ubuntu/git/aoc_2021/01/input.txt'"
       self loadPuzzleInput: filenameString.
       ^ self calculatePart1! !

!Day01 methodsFor: 'accessing' stamp: 'pab 6/25/2022 23:38:41'!
loadPuzzleInput: aString
	"Load the file containing the input data"
	depths removeAll.
	aString asFileEntry readStreamDo: [ :fileStream | [ fileStream atEnd ]
		whileFalse: [ depths add: (fileStream upTo: Character lf) asNumber ] ].
	^ depths size
	! !

!Day01 methodsFor: 'as yet unclassified' stamp: 'pab 6/25/2022 23:32:46'!
initialize
	"Set instance variables"
       super initialize.
	count := 0.
	depths := OrderedCollection new.! !
