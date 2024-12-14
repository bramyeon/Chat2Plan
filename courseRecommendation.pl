/*================================================================================

Helper Script for CS579 Term Project (Fall 2024)
Project Title: 'Chat2Plan: Course Planner Chatbot for KAIST SoC Students'
Feature: courseRecommendation
Compiled by Bryan Nathanael Wijaya (20244078)

================================================================================*/

:- module(courseRecommendation,[askMC/1,
                                filterKeyRecom/3]).

:- use_module(coursePlanning,[collectKey/2,
                                extractFirst/2,
                                extractFirstWord/2,
                                filterKey/3,
                                intersection/3,
                                sortByScore/2]).

:- use_module(courseKB,[course/11]).

:- use_module(lambda,[lambda/2]).

:- use_module(readLine,[readLine/1]).

/*========================================================================
   Score courses based on user's keywords of interest (like in coursePlanning)
   and remove courses if the course has no keyword intersection with user's
   keywords of interest
========================================================================*/

filterKeyRecom([Sems],InitCourses,FinCourses):-
    collectKey(Sems,UserKeys),
    filterKey([Sems],InitCourses,Courses),
    strictFilter(UserKeys,Courses,FinCourses).


/* Remove courses if there is no keyword intersection*/
strictFilter(_,[],[]).

strictFilter(UserKeys,[(CourseNum,Score)|T],[(CourseNum,Score)|ResT]):-
    course(CourseNum,_,_,_,_,_,_,_,_,Keywords,_),
    intersection(Keywords,UserKeys,Ints),
    length(Ints,NumInts),
    NumInts>0,
    strictFilter(UserKeys,T,ResT).

strictFilter(UserKeys,[(CourseNum,_)|T],ResT):-
    course(CourseNum,_,_,_,_,_,_,_,_,Keywords,_),
    intersection(Keywords,UserKeys,Ints),
    length(Ints,NumInts),
    \+ NumInts>0,
    strictFilter(UserKeys,T,ResT).


/*========================================================================
   Sort the courses based on keyword scores, 
   give a course recommendation (the first on the list), and 
   ask if the user wants more recommendations
========================================================================*/

askMC(CourseList):-
    sortByScore(CourseList,SortedList),
    extractFirst(SortedList,CourseNums),
    askMCIter(CourseNums).


/* Helper to print the head course number and ask if user wants more */
askMCIter([CourseNumSub|T]):-
    course(CourseNumSub,CourseTitle,CourseSubtitle,_,_,_,_,_,_,_,_),
    string_upper(CourseNumSub,UpperNum),
    (
        CourseSubtitle='nan',
        string_concat(CourseTitle,' (',TitleStr1),
        string_concat(TitleStr1,UpperNum,TitleStr2),
        string_concat(TitleStr2,')',TitleStr),!
    ;
        extractFirstWord(UpperNum,CourseNum),
        string_concat(CourseTitle,': ',TitleStr1),
        string_concat(TitleStr1,CourseSubtitle,TitleStr2),
        string_concat(TitleStr2,' (',TitleStr3),
        string_concat(TitleStr3,CourseNum,TitleStr4),
        string_concat(TitleStr4,')',TitleStr)
    ),
    nl,format('I think ~w would be a good fit for you. Would you like another recommendation? (yes/no)',[TitleStr]),nl,
    moreRecoms(Bool),
    (
        Bool=true,
        askMCIter(T),!
    ;
        Bool=false,
        true
    ;
        nl,format('Sorry! I ran out of recommendations. Maybe try more keywords?'),nl
    ).


/* Helper to handle user's answer to whether he/she wants more recommendations */
moreRecoms(Bool):-
    readLine(MoreStr),
    (
        (
            MoreStr=[yes],!;
            MoreStr=[y]
        ),
        Bool=true
    ;
        (
            MoreStr=[no],!;
            MoreStr=[n]
        ),
        Bool=false
    ).
