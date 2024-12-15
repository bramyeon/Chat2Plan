/*================================================================================

Main Script for CS579 Term Project (Fall 2024) - GRADIO VERSION
Project Title: 'Chat2Plan: Course Planner Chatbot for KAIST SoC Students'

The predicates here are adapted from our main source codes simply to be used 
in the Gradio UI because pyswip does not support capturing inputs from readLine/1
and its variations and forwarding strings outputted from write/1 and format/2, 
also to adjust with the format requirements of Gradio Chatbot. 
No "new" operations are defined, so the output of the systems in main.pl and
the Gradio version are identical by design.

Compiled by Bryan Nathanael Wijaya (20244078)

================================================================================*/

:- module(mainGradio,[askCNumGradio/2,
                    askNameGradio/2,
                    askTaskGradio/2,
                    filterEnrollGradio/2,
                    filterEnrollLoadGradio/2,
                    filterKeyGradio/3,
                    filterKeyRecomGradio/3,
                    filterListGradio/3,
                    filterMustGradio/4,
                    filterNumGradio/2,
                    filterPrevGradio/3,
                    filterSemGradio/3,
                    printCourseGradio/2,
                    retrieveCourseGradio/2]).

:- use_module(courseKB,[course/11]).

:- use_module(coursePlanning,[extractFirst/2,
                                extractFirstWord/2,
                                filterEnroll/2,
                                filterKey/3,
                                filterMust/4,
                                filterPrev/3,
                                filterSem/3,
                                printPlanIter/2,
                                sortByScore/2]).

:- use_module(loadApproximation,[filterEnrollLoad/2,
                                    filterList/3]).

:- use_module(courseRecommendation,[filterKeyRecom/3]).

:- use_module(courseInformation,[getCourseNum/2]).

:- use_module(lambda,[lambda/2]).

/*========================================================================
   Main function helpers upon initiation
========================================================================*/

/* Ask user's name */
askNameGradio(Sentence,Name):-
    Sentence=[_,_,_|[Name|_]].

/* Ask which feature to do */
askTaskGradio(Sentence,Action):-
    lambda(Sentence,Sems),!,
    Sems=[want(user,Action)];
    false.

/*========================================================================
   Course planning: Give a list of course recommendations
========================================================================*/

filterEnrollGradio(Sentence,Courses):-
    lambda(Sentence,Sems),!,
    filterEnroll(Sems,Courses);
    false.


filterSemGradio(Sentence,InitCourses,FinCourses):-
    lambda(Sentence,Sems),!,
    filterSem(Sems,InitCourses,FinCourses);
    false.


filterKeyGradio(Sentence,InitCourses,FinCourses):-
    lambda(Sentence,Sems),!,
    filterKey(Sems,InitCourses,FinCourses);
    false.


filterPrevGradio(Sentence,InitCourses,FinCourses):-
    (
        Sentence=[no],Sems=[none],!;
        lambda(Sentence,Sems)
    ),
    filterPrev(Sems,InitCourses,FinCourses),!;
    false.


filterNumGradio(Sentence,CourseNums):-
    lambda(Sentence,Sems),!,
    Sems=[planTake(user,CourseNums)];
    false.


filterMustGradio(Sentence,CourseNums,InitCourses,CourseStr):-
    (
        Sentence=[no],Sems=[none],!;
        lambda(Sentence,Sems)
    ),
    filterMust(Sems,CourseNums,InitCourses,FinCourses),!,
    printPlanGradio(FinCourses,CourseStr);
    false.


printPlanGradio(CourseNums,CourseStr):-
    printPlanIter(CourseNums,CourseNumStr),
    atomic_list_concat(CourseNumStr,', ',TitleStrs),
    string_concat('I recommend you to take ',TitleStrs,CourseStr1),
    string_concat(CourseStr1,' this semester.',CourseStr).


/*========================================================================
   Load approximation: Approximate the overall load of a course plan
========================================================================*/

filterEnrollLoadGradio(Sentence,LoadMax):-
    lambda(Sentence,Sems),!,
    filterEnrollLoad(Sems,LoadMax).


filterListGradio(Sentence,CourseNums,Load):-
    lambda(Sentence,Sems),!,
    filterList(Sems,CourseNums,Load).


/*========================================================================
   Course recommendation: Recommend a course based on user's interest
========================================================================*/

filterKeyRecomGradio(Sentence,InitCourses,FinCourses):-
    lambda(Sentence,Sems),!,
    filterKeyRecom(Sems,InitCourses,CourseList),
    sortByScore(CourseList,SortedList),
    extractFirst(SortedList,FinCourses).


retrieveCourseGradio(CourseNumSub,CourseStr):-
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
    string_concat('I think ',TitleStr,CourseStr1),
    string_concat(CourseStr1,' would be a good fit for you. Would you like another recommendation? (yes/no)',CourseStr).
    

/*========================================================================
   Course information: Get the info of a course
========================================================================*/

askCNumGradio(Sentence,CourseNum):-
	(
        Sentence=[tell,me,everything,about|Syntax],
        getCourseNum(Syntax,CourseNum),!
    ;
        CourseNum=false
    ).


printCourseGradio(CourseNumSub,CourseStr):-
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
        string_concat(CourseTitle,': ',TitleStr1),
        string_concat(TitleStr1,CourseSubtitle,TitleStr2),
        string_concat(TitleStr2,' (',TitleStr3),
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
    string_concat(TitleStr,' is ',CourseStr1),
    string_concat(CourseStr1,TypeStr,CourseStr2),
    string_concat(CourseStr2,' course in KAIST SoC that is ',CourseStr3),
    string_concat(CourseStr3,SemesterStr,CourseStr4),
    string_concat(CourseStr4,'. This course is ',CourseStr5),
    string_concat(CourseStr5,MutualStr,CourseStr6),
    string_concat(CourseStr6,'. ',CourseStr7),
    string_concat(CourseStr7,KeywordsStr,CourseStr8),
    string_concat(CourseStr8,'. ',CourseStr9),
    string_concat(CourseStr9,ReqStr,CourseStr10),
    string_concat(CourseStr10,'. ',CourseStr11),
    string_concat(CourseStr11,RecStr,CourseStr12),
    string_concat(CourseStr12,'. The relative load of this course is ',CourseStr13),
    string_concat(CourseStr13,CourseLoad,CourseStr14),
    string_concat(CourseStr14,'/5.',CourseStr).
