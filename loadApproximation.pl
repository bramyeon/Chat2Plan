/*================================================================================

Helper Script for CS579 Term Project (Fall 2024)
Project Title: 'Chat2Plan: Course Planner Chatbot for KAIST SoC Students'
Feature: loadApproximation
Compiled by Bryan Nathanael Wijaya (20244078)

================================================================================*/

:- module(loadApproximation,[askList/1,
                                filterEnrollLoad/2,
                                filterList/3,
                                printLoad/3]).

:- use_module(courseKB,[course/11]).

:- use_module(coursePlanning,[retry/1]).

:- use_module(lambda,[lambda/2]).

:- use_module(readLine,[readLine/1]).

/*========================================================================
   Set course load maximum threshold based on user's enrollment information:
   - Undergraduate: 18
   - Graduate: 10
========================================================================*/

filterEnrollLoad([Sems],LoadMax):-
    evalEnrollLoad(Sems,LoadMax).


/* Set load threshold depending on user's enrollment information */
evalEnrollLoad(some(_,and(undergraduate(_),eq(user,_))),18).
evalEnrollLoad(some(_,and(graduate(_),eq(user,_))),10).


/*========================================================================
   Ask for the user's list of courses to take 
   (course number, or together with subtitle for special topics)
========================================================================*/

askList(Sems):-
    nl,write('Now, could you let me know all the SoC courses you that plan to take?'),nl,
    write('Please separate the course numbers with "and" and for special topics, please include the course subtitle.'),nl,
    write('An example response would be "I am taking CS101 and CS376 and CS492 Diffusion Models and Their Applications"'),nl,
	readLine(Sentence),!,
	(
        lambda(Sentence,Sems),!
    ;
        nl,write('I can\'t understand you. Please try again...'),nl,
        retry(Sems)
    ).


/*========================================================================
   Accumulate the number of courses being taken and the cumulative load
========================================================================*/

filterList([Sems],CourseNums,Load):-
    collectCourses(Sems,Courses),
    length(Courses,CourseNums),
    collectLoad(Courses,Load).


/* Helper to extract the list of course numbers */
collectCourses(taking(user,Course),[Course]).

collectCourses(and(Left,Right),Courses):-
    collectCourses(Left,LCourses),
    collectCourses(Right,RCourses),
    append(LCourses,RCourses,Courses).


/* Helper to sum up the course load from the list 
   by checking the course knowledge base*/
collectLoad([],0).

collectLoad([CourseNum|T],TotalLoad):-
    course(CourseNum,_,_,_,_,_,_,_,_,_,Load),
    collectLoad(T,LoadT),
    TotalLoad is Load+LoadT.


/*========================================================================
   Print the course load approximation
========================================================================*/

printLoad(CourseNums,Load,LoadMax):-
    (
        Load>LoadMax,
        nl,format('Holy moly! You have ~w classes with a total load score of ~w, where the optimum load for your program is ~w. Please considering dropping some of them.',[CourseNums,Load,LoadMax]),nl
    ;
        nl,format('Looking good! You have ~w classes with a total load score of ~w, where the optimum load for your program is ~w.',[CourseNums,Load,LoadMax]),nl
    ).