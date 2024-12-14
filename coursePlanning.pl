/*================================================================================

Helper Script for CS579 Term Project (Fall 2024)
Project Title: 'Chat2Plan: Course Planner Chatbot for KAIST SoC Students'
Feature: coursePlanning
Compiled by Bryan Nathanael Wijaya (20244078)

================================================================================*/

:- module(coursePlanning,[askEnroll/1,
                            askKey/1,
                            askMust/1,
                            askNum/1,
                            askPrev/1,
                            askSem/1,
                            collectKey/2,
                            extractFirst/2,
                            extractFirstWord/2,
                            filterEnroll/2,
                            filterKey/3,
                            filterMust/4,
                            filterNum/2,
                            filterPrev/3,
                            filterSem/3,
                            intersection/3,
                            printPlan/1,
                            printPlanIter/2,
                            retry/1,
                            sortByScore/2]).

:- use_module(courseKB,[course/11]).

:- use_module(lambda,[lambda/2]).

:- use_module(readLine,[readLine/1]).

/*========================================================================
   Ask for the user's enrollment information
========================================================================*/

askEnroll(Sems):-
    nl,format('Okay, let\'s begin! Which program are you currently enrolled in? Undergraduate or graduate?'),nl,
    write('An example response would be "I am a graduate student"'),nl,
    readLine(Sentence),!,
	(
        lambda(Sentence,Sems),!
    ;
        nl,write('I can\'t understand you. Please try again...'),nl,
        retry(Sems)
    ).


/*========================================================================
   Filter courses based on user's enrollment information
========================================================================*/

filterEnroll([Sems],Courses):-
    findall(CourseNum,evalEnroll(Sems,CourseNum),Courses).


/* Case 1: User is an undergraduate student */
evalEnroll(some(_,and(undergraduate(_),eq(user,_))),CourseNum):-
    (
        course(CourseNum,_,_,CourseType,false,_,_,_,_,_,_),
        \+ CourseType='Elective(Graduate)'
    ;
        course(CourseNum,_,_,_,true,_,_,_,_,_,_)
    ).


/* Case 2: User is a graduate student */
evalEnroll(some(_,and(graduate(_),eq(user,_))),CourseNum):-
    (
        course(CourseNum,_,_,_,true,_,_,_,_,_,_);
        course(CourseNum,_,_,'Elective(Graduate)',false,_,_,_,_,_,_)
    ).


/*========================================================================
   Ask for the user's desired semester
========================================================================*/

askSem(Sems):-
    nl,write('Awesome! For which semester are we planning your courses? Spring or fall?'),nl,
    write('An example response would be "I want to plan for the spring semester"'),nl,
	readLine(Sentence),!,
	(
        lambda(Sentence,Sems),!
    ;
        nl,write('I can\'t understand you. Please try again...'),nl,
        retry(Sems)
    ).


/*========================================================================
   Filter courses based on user's desired semester
========================================================================*/

filterSem([Sems],InitCourses,FinCourses):-
    findall(CourseNum,evalSem(Sems,CourseNum),Courses),
    intersection(InitCourses,Courses,FinCourses).


/* Case 1: User is planning for a spring semester */
evalSem(plan(user,spring),CourseNum):-
    course(CourseNum,_,_,_,_,OpeningSemester,_,_,_,_,_),
    \+ OpeningSemester='fall'.


/* Case 2: User is planning for a fall semester */
evalSem(plan(user,fall),CourseNum):-
    course(CourseNum,_,_,_,_,OpeningSemester,_,_,_,_,_),
    \+ OpeningSemester='spring'.


/*========================================================================
   Ask for the user's keywords of interest
========================================================================*/

askKey(Sems):-
    nl,write('Now, what topics are you interested in?'),nl,
    format('You can choose from the following list and separate with "and":~n  - AI-Information Service / AI~n  - Algorithms~n  - Computer Architecture~n  - Computing Philosophy~n  - Computing Theory~n  - Data Science~n  - Data Structure~n  - Digital System~n  - Entrepreneurship~n  - Ethics~n  - Interactive Computing~n  - Machine Learning / ML~n  - Natural Language Processing / NLP~n  - Networking~n  - Operating Systems / OS~n  - Programming Basics~n  - Programming Language / PL~n  - Research Methodology~n  - Robotics~n  - Secure Computing~n  - Social Computing~n  - Software Design~n  - Systems-Networks~n  - Technical Writing~n  - Visual Computing / Computer Vision / CV'),nl,
    write('An example response would be "I am interested in Algorithms and NLP and Machine Learning"'),nl,
	readLine(Sentence),!,
	(
        lambda(Sentence,Sems),!
    ;
        nl,write('I can\'t understand you. Please try again...'),nl,
        retry(Sems)
    ).


/*========================================================================
   Score the courses based on user's keywords of interest and 
   the course opening (regular or irregular)
========================================================================*/

filterKey([Sems],InitCourses,FinCourses):-
    collectKey(Sems,UserKeys),
    evalKey(InitCourses,UserKeys,FinCourses).


/* Helper to extract user's keywords of interest */ 
collectKey(interest(user,Key),[KeyTrans]):-
    convKey(Key,KeyTrans).


collectKey(and(Left,Right),Keys):-
    collectKey(Left,LKeys),
    collectKey(Right,RKeys),
    append(LKeys,RKeys,Keys).


/* Helper to convert user's lexicon's symbols to natural language keywords */
convKey(ainfos,'AI-Information Service').
convKey(algos,'Algorithms').
convKey(archi,'Computer Architecture').
convKey(philo,'Computing Philosophy').
convKey(theory,'Computing Theory').
convKey(datasci,'Data Science').
convKey(datastr,'Data Structure').
convKey(digisys,'Digital System').
convKey(entrep,'Entrepreneurship').
convKey(ethics,'Ethics').
convKey(intcom,'Interactive Computing').
convKey(ml,'Machine Learning').
convKey(nlp,'Natural Language Processing').
convKey(network,'Networking').
convKey(os,'Operating Systems').
convKey(progbas,'Programming Basics').
convKey(proglang,'Programming Language').
convKey(rmethod,'Research Methodology').
convKey(robotics,'Robotics').
convKey(seccom,'Secure Computing').
convKey(soccom,'Social Computing').
convKey(swdes,'Software Design').
convKey(sysnet,'Systems-Networks').
convKey(writing,'Technical Writing').
convKey(cv,'Visual Computing').


/* Helper to calculate the score of each course based on keywords and regular opening:
   +2 for each keyword match and +2 if the course is regularly opened */
evalKey([],_,[]).

evalKey([CourseNum|T],UserKeys,[(CourseNum,Score)|ResultT]) :-
    course(CourseNum,_,_,_,_,_,RegularOpening,_,_,Keywords,_),
    intersection(Keywords,UserKeys,Ints),
    length(Ints,NumInts),
    (
        RegularOpening=true,
        Bonus is 2,!
    ;
        Bonus is 0
    ),
    Score is 2 * NumInts + Bonus,
    evalKey(T,UserKeys,ResultT).


/*========================================================================
   Ask for the user's previously taken courses
========================================================================*/

askPrev(Sems):-
    nl,write('Perfect! This might be a bit annoying, but, could you let me know all the SoC courses you have previously taken?'),nl,
    write('Please separate the course numbers with "and" and for special topics, please include the course subtitle.'),nl,
    write('An example response would be "I took CS101 and CS376 and CS492 Diffusion Models and Their Applications", or if you haven\'t take any courses from the SoC, simply say "No"'),nl,
	readLine(Sentence),!,
	(
        Sentence=[no],
        Sems=[none],!
    ;
        lambda(Sentence,Sems),!
    ;
        nl,write('I can\'t understand you. Please try again...'),nl,
        retry(Sems)
    ).


/*========================================================================
   Filter and rank based on the user's previously taken courses:
   - Remove courses that the user has previously taken
   - Remove courses which required prerequisites have not been taken
   - Score courses based on the recommended prerequisites: 
     +1 for each match btw previously taken courses and recommended prerequisites
========================================================================*/

filterPrev([Sems],InitCourses,FinCourses):-
    (
        Sems=none,
        UserPrevs=[],!
    ;
        collectPrev(Sems,UserPrevs)
    ),
    differenceTuple(InitCourses,UserPrevs,Courses),
    evalPrev(Courses,UserPrevs,FinCourses).


/* Helper to extract user's previously taken courses */ 
collectPrev(took(user,Prev),[Prev]).

collectPrev(and(Left,Right),Prevs):-
    collectPrev(Left,LPrevs),
    collectPrev(Right,RPrevs),
    append(LPrevs,RPrevs,Prevs).


/* Helper to filter and rank courses based on prerequisites:
   - Remove courses which required prerequisites have not been taken
   - Score courses based on the recommended prerequisites: 
     +1 for each match btw previously taken courses and recommended prerequisites */ 
evalPrev([],_,[]).

/* Case 1: All required prerequisites are fulfilled */
evalPrev([(CourseNum,Score)|T],UserPrevs,[(CourseNum,NewScore)|ResultT]) :-
    course(CourseNum,_,_,_,_,_,_,ReqPres,RecPres,_,_),
    lowerList(ReqPres,LowerReqPres),
    lowerList(RecPres,LowerRecPres),
    difference(LowerReqPres,UserPrevs,ReqDiffs),
    length(ReqDiffs,NumReqDiffs),
    NumReqDiffs=0,
    intersection(LowerRecPres,UserPrevs,RecInts),
    length(RecInts,NumRecInts),
    NewScore is Score + NumRecInts,
    evalPrev(T,UserPrevs,ResultT).

/* Case 2: Some required prerequisites are not fulfilled */
evalPrev([(CourseNum,_)|T],UserPrevs,ResultT) :-
    course(CourseNum,_,_,_,_,_,_,ReqPres,_,_,_),
    lowerList(ReqPres,LowerReqPres),
    difference(LowerReqPres,UserPrevs,ReqDiffs),
    length(ReqDiffs,NumReqDiffs),
    \+ NumReqDiffs=0,
    evalPrev(T,UserPrevs,ResultT).


/*========================================================================
   Ask for the user's desired number of courses
========================================================================*/

askNum(Sems):-
    nl,write('We\'re almost there! How many classes do you plan to take next semester?'),nl,
    write('An example response would be "I plan to take 5 courses"'),nl,
	readLine(Sentence),!,
	(
        lambda(Sentence,Sems),!
    ;
        nl,write('I can\'t understand you. Please try again...'),nl,
        retry(Sems)
    ).


/*========================================================================
   Extract the user's desired number of courses
========================================================================*/

filterNum([planTake(user,CourseNums)],CourseNums).


/*========================================================================
   Ask for the user's must-take courses for the semester
========================================================================*/

askMust(Sems):-
    nl,write('Finally, is there any course that you must take next semester? (yes/no)'),nl,
    write('If yes, separate the course numbers with "and".'),nl,
    write('Some example responses would be "Yes, I must take CS330 and CS320" or "No"'),nl,
	readLine(Sentence),!,
	(
        Sentence=[no],
        Sems=[none],!
    ;
        lambda(Sentence,Sems),!
    ;
        nl,write('I can\'t understand you. Please try again...'),nl,
        retry(Sems)
    ).


/*========================================================================
   Sort the courses based on the score, 
   add the user's must-take courses for the semester to the front,
   and return courses based on the user's desired number of courses
========================================================================*/

filterMust([Sems],CourseNums,InitCourses,FinCourses):-
    (
        Sems=none,
        MustList=[],!
    ;
        collectMust(Sems,MustList)
    ),
    sortByScore(InitCourses,Courses),
    extractFirst(Courses,TempCourses),
    addToFront(MustList,TempCourses,EditCourses),
    cutList(CourseNums,EditCourses,FinCourses).


/* Extract the user's must-take courses */
collectMust(mustTake(user,Must),[Must]).

collectMust(and(Left,Right),Musts):-
    collectMust(Left,LMusts),
    collectMust(Right,RMusts),
    append(LMusts,RMusts,Musts).


/* Sort courses based on the scores (second element of tuple) */
sortByScore(List,SortedList):-
    predsort(compareSecond,List,SortedList).

/* Helper for sorting:
   - Sort in descending order of score
   - If a tie, sort in ascending order of course number */
compareSecond(Order,(CourseNum1,Score1),(CourseNum2,Score2)):-
    \+ CourseNum1=CourseNum2,
    (
        Score1 > Score2 -> Order='<';
        Score1 < Score2 -> Order='>';
        CourseNum1@<CourseNum2 -> Order='<';
        Order='>'
    ).


/* Extract the first element of tuple in the list,
   i.e., remove the scores after sorting */
extractFirst([],[]).

extractFirst([(CourseNum,_)|T],[CourseNum|CourseNums]) :-
    extractFirst(T,CourseNums).


/* Add the user's must-take courses to the front of the list */
addFront(Course,List,[Course|List]).

addToFront([],List,List).
addToFront([H|T],List,Result):-
    addFront(H,List,TempList),
    addToFront(T,TempList,Result).


/* Get the first N elements of the list, where 
   N is the user's desired number of courses */
cutList(0,_,[]):-!.

cutList(_,[],[]):-!.

cutList(N,[H|T],[H|Result]):-
    N>0,
    N1 is N-1,
    cutList(N1,T,Result).


/*========================================================================
   Print the course plan: Course title (Course number)
========================================================================*/

printPlan(CourseNums):-
    printPlanIter(CourseNums,CourseNumStr),
    atomic_list_concat(CourseNumStr,', ',TitleStrs),
    nl,format('I recommend you take ~w this semester.', [TitleStrs]),nl.


/* Helper function to grab the course numbers of the plan */
printPlanIter([],[]).

printPlanIter([H|T],[HStr|TStr]):-
    printPlanCourse(H,HStr),
    printPlanIter(T,TStr).


/* Helper function to get the string of course number:
   - Normal course: Only course number
   - Special topics: Course number and subtitle */
printPlanCourse(CourseNumSub,TitleStr):-
    course(CourseNumSub,CourseTitle,CourseSubtitle,_,_,_,_,_,_,_,_),
    string_upper(CourseNumSub,UpperNum),
    (
        CourseSubtitle='nan',
        string_concat(CourseTitle,' (',TitleStr1),
        string_concat(TitleStr1,UpperNum,TitleStr2),
        string_concat(TitleStr2,')',TitleStr),!
    ;
        extractFirstWord(UpperNum,CourseNum),
        string_concat(CourseTitle,'<',TitleStr1),
        string_concat(TitleStr1,CourseSubtitle,TitleStr2),
        string_concat(TitleStr2,'> (',TitleStr3),
        string_concat(TitleStr3,CourseNum,TitleStr4),
        string_concat(TitleStr4,')',TitleStr)
    ).


/* Helper function to get the first word (course number) from the lexical symbol 
   for special topic courses (which contains course number + subtitle) */
extractFirstWord(String,FirstWord):-
    split_string(String," ","",Words),
    Words=[FirstWord|_].


/*========================================================================
   Miscellaneous helper functions
========================================================================*/

/* Helper to handle undesired input format */
retry(Sems):-
    readLine(Sentence),!,
    (
        lambda(Sentence,Sems),!
    ;
        nl,write('I can\'t understand you. Please try again...'),nl,
        retry(Sems)
    ).


/* Helper to find the intersection between two lists */
intersection([],_,[]).

intersection([Head|Tail],L2,[Head|L3]) :-
    member(Head,L2),
    intersection(Tail,L2,L3).

intersection([Head|Tail],L2,L3) :-
    \+ member(Head,L2),
    intersection(Tail,L2,L3).


/* Helper to find the difference between two lists */
difference([],_,[]).

difference([Head|Tail],L2,[Head|L3]):-
    \+ member(Head,L2),
    difference(Tail,L2,L3).

difference([Head|Tail],L2,L3):-
    member(Head,L2),
    difference(Tail,L2,L3).


/* Helper to find the difference between two lists when
   the first list is a tuple of (CourseNum, Score) */
differenceTuple([],_,[]).

differenceTuple([(CourseNum,Score)|Tail],L2,[(CourseNum,Score)|L3]):-
    \+ member(CourseNum,L2),
    differenceTuple(Tail,L2,L3).

differenceTuple([(CourseNum,_)|Tail],L2,L3):-
    member(CourseNum,L2),
    differenceTuple(Tail,L2,L3).


/* Helper to change the list elements to lowercase */
lowerList([],[]).

lowerList([CourseNum|T],[LowerNum|LowerT]):-
    string_lower(CourseNum,LowerNum),
    lowerList(T,LowerT).
