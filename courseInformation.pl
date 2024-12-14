/*================================================================================

Helper Script for CS579 Term Project (Fall 2024)
Project Title: 'Chat2Plan: Course Planner Chatbot for KAIST SoC Students'
Feature: courseInformation
Compiled by Bryan Nathanael Wijaya (20244078)

================================================================================*/

:- module(courseInformation,[askCNum/1,
                                getCourseNum/2,
                                printCourse/1]).

:- use_module(coursePlanning,[extractFirstWord/2]).

:- use_module(courseKB,[course/11]).

:- use_module(readLine,[readLine/1]).

/*========================================================================
   Ask for a course number to investigate
========================================================================*/

askCNum(CourseNum):-
    nl,write('Which course would you like to know about?'),nl,
    write('Some example responses would be "Tell me everything about CS579" or "Tell me everything about CS492 Unconventional Computing"'),nl,
	readLine(Sentence),!,
	(
        Sentence=[tell,me,everything,about|Syntax],
        getCourseNum(Syntax,CourseNum),!
    ;
        nl,write('I can\'t understand you. Please try again...'),nl,
        retryNum(CourseNum)
    ).


/* Helper to extract the course number */
getCourseNum([H|T],CourseNum):-
    (
        T=[],
        CourseNum=H,!
    ;
        atomic_list_concat([H|T],' ',CourseNum)
    ).


/* Helper to handle undesired input format */
retryNum(CourseNum):-
    readLine(Sentence),!,
    (
        Sentence=[tell,me,everything,about|Syntax],
        getCourseNum(Syntax,CourseNum),!
    ;
        nl,write('I can\'t understand you. Please try again...'),nl,
        retryNum(CourseNum)
    ).


/*========================================================================
   Print course information
========================================================================*/

printCourse(CourseNumSub):-
    course(CourseNumSub,CourseTitle,CourseSubtitle,CourseType,MutualRecognition,OpeningSemester,RegularOpening,RequiredPrerequisites,RecommendedPrerequisites,Keywords,CourseLoad),
    string_upper(CourseNumSub,UpperNum),
    string_lower(CourseType,LowerType),
    (
        CourseType='Elective(Graduate)',
        TypeStr='an elective (graduate)',!
    ;
        string_concat('a ',LowerType,TypeStr)
    ),
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
    ),
    (
        MutualRecognition=true,
        MutualStr='mutually recognized for both undergraduate and graduate students',!
    ;
        MutualRecognition=false,
        CourseType='Elective(Graduate)',
        MutualStr='only for graduate students',!
    ;
        MutualStr='only for undergraduate students'
    ),
    (
        RegularOpening=true,
        string_concat('offered regularly every ',OpeningSemester,SemesterStr),!
    ;
        string_concat('occasionally offered in ',OpeningSemester,SemesterStr)
    ),
    (
        Keywords=[KeyH|[]],
        string_concat('A keyword of this course is ',KeyH,KeywordsStr),!
    ;
        atomic_list_concat(Keywords,', ',KeyStr),
        string_concat('Some keywords of this course are ',KeyStr,KeywordsStr)
    ),
    (
        RequiredPrerequisites=[],
        ReqStr='There is no required prerequisites to this course',!
    ;
        RequiredPrerequisites=[ReqH|[]],
        string_concat('The required prerequisite of this course is ',ReqH,ReqStr),!
    ;
        atomic_list_concat(RequiredPrerequisites,', ',ReqList),
        string_concat('The required prerequisites of this course are ',ReqList,ReqStr)
    ),
    (
        RecommendedPrerequisites=[],
        RecStr='There is no recommended prerequisites to this course',!
    ;
        RecommendedPrerequisites=[RecH|[]],
        string_concat('The recommended prerequisite of this course is ',RecH,RecStr),!
    ;
        atomic_list_concat(RecommendedPrerequisites,', ',RecList),
        string_concat('The recommended prerequisites of this course are ',RecList,RecStr)
    ),
    nl,format('~w is ~w course in KAIST SoC that is ~w. This course is ~w. ~w. ~w. ~w. The relative load of this course is ~w/5.',[TitleStr,TypeStr,SemesterStr,MutualStr,KeywordsStr,ReqStr,RecStr,CourseLoad]),nl.