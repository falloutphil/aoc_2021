'From Cuis 6.0 [latest update: #5299] on 29 June 2022 at 9:38:54 am'!
'Description '!
!provides: 'AOC2021' 1 6!
SystemOrganization addCategory: 'AOC2021'!


!classDefinition: #Day01Sub category: 'AOC2021'!
ImageMorph subclass: #Day01Sub
	instanceVariableNames: 'depthCoords'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AOC2021'!
!classDefinition: 'Day01Sub class' category: 'AOC2021'!
Day01Sub class
	instanceVariableNames: ''!

!classDefinition: #Day01Morph category: 'AOC2021'!
PlacedMorph subclass: #Day01Morph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AOC2021'!
!classDefinition: 'Day01Morph class' category: 'AOC2021'!
Day01Morph class
	instanceVariableNames: ''!

!classDefinition: #Day01Line category: 'AOC2021'!
Morph subclass: #Day01Line
	instanceVariableNames: 'depthCoords'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AOC2021'!
!classDefinition: 'Day01Line class' category: 'AOC2021'!
Day01Line class
	instanceVariableNames: ''!

!classDefinition: #Day01 category: 'AOC2021'!
Object subclass: #Day01
	instanceVariableNames: 'depths count'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AOC2021'!
!classDefinition: 'Day01 class' category: 'AOC2021'!
Day01 class
	instanceVariableNames: ''!


!Day01Sub methodsFor: 'as yet unclassified' stamp: 'pa 6/28/2022 22:49:17'!
addDepths: anOrderedCollection
	depthCoords := anOrderedCollection withIndexCollect: [ :e :i | i - 40 @ ((e/3) - 100) ]! !

!Day01Sub methodsFor: 'as yet unclassified' stamp: 'pa 6/28/2022 22:30:42'!
initialize
	"comment stating purpose of message"
	super initialize.
	self image: (ImageReadWriter formFromFileEntry: ('/home/ubuntu/git/aoc_2021/01/smallsub.png' asFileEntry)).! !

!Day01Sub methodsFor: 'as yet unclassified' stamp: 'pa 6/28/2022 22:19:18'!
step
	self morphPosition: depthCoords removeFirst.
	(depthCoords size = 0) ifTrue: [ self stopStepping ].
	self redrawNeeded! !

!Day01Sub methodsFor: 'as yet unclassified' stamp: 'pa 6/28/2022 19:34:55'!
stepTime
	^ 100! !

!Day01Sub methodsFor: 'as yet unclassified' stamp: 'pa 6/28/2022 22:24:14'!
wantsSteps
	^ true! !

!Day01Sub class methodsFor: 'as yet unclassified' stamp: 'pa 6/28/2022 21:41:44'!
with: anOrderedCollection
	^ self new
		addDepths: anOrderedCollection;
		yourself! !

!Day01Morph methodsFor: 'as yet unclassified' stamp: 'pa 6/28/2022 21:11:06'!
drawOn: aCanvas
	aCanvas
		strokeWidth: 1
		color: Color blue
		fillColor: (Color blue alpha: 0.5)
		do: [
			aCanvas moveTo: 0 @ 0;
			lineTo: 2000 @ 0;
			lineTo: 2000 @ 1300;
			lineTo: 0 @ 1300;
			lineTo: 0 @ 0]! !

!Day01Morph methodsFor: 'geometry testing' stamp: 'pa 6/28/2022 23:01:15'!
clipsSubmorphs
	^ true! !

!Day01Morph class methodsFor: 'as yet unclassified' stamp: 'pa 6/28/2022 09:08:10'!
with: anOrderedCollection
	^ self new
		addMorph: (Day01Line with: anOrderedCollection);
		addMorph: (Day01Sub with: anOrderedCollection);
		yourself! !

!Day01Line methodsFor: 'drawing' stamp: 'pa 6/29/2022 09:37:53'!
drawOn: aCanvas
	"comment stating purpose of message"
	aCanvas strokeWidth: 5 color: (Color r: 249 g: 232 b: 182) fillColor: (Color darkGray) do: [
		aCanvas moveTo: 0 @500.
		depthCoords do: [ :c | aCanvas lineTo: c ].
		aCanvas
			lineTo: 0@(3728/3 + 50);
			lineTo: 0@500.
		].! !

!Day01Line methodsFor: 'as yet unclassified' stamp: 'pa 6/28/2022 22:59:50'!
addDepths: anOrderedCollection
	depthCoords := anOrderedCollection withIndexCollect: [ :e :i | i @ ((e/3) + 50) ]! !

!Day01Line class methodsFor: 'as yet unclassified' stamp: 'pa 6/26/2022 23:11:06'!
with: anOrderedCollection
	^ self new
		addDepths: anOrderedCollection;
		yourself! !

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

!Day01 methodsFor: 'as yet unclassified' stamp: 'pa 6/26/2022 23:25:34'!
show
	"comment stating purpose of message"
	(Day01Morph with: depths) openInWorld! !
