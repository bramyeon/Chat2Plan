/*================================================================================

Main Script for CS579 Term Project (Fall 2024)
Project Title: 'Chat2Plan: Course Planner Chatbot for KAIST SoC Students'
Compiled by Bryan Nathanael Wijaya (20244078)

================================================================================*/

:- module(main,[chat2plan/0,
                    courseInformation/0,
                    coursePlanning/0,
                    courseRecommendation/0,
                    info/0,
                    loadApproximation/0]).

:- use_module(courseKB,[course/11]).

:- use_module(coursePlanning,[askEnroll/1,
                                askKey/1,
                                askPrev/1,
                                askMust/1,
                                askNum/1,
                                askSem/1,
                                filterEnroll/2,
                                filterKey/3,
                                filterMust/4,
                                filterNum/2,
                                filterPrev/3,
                                filterSem/3,
                                printPlan/1,
                                retry/1]).

:- use_module(loadApproximation,[askList/1,
                                    filterEnrollLoad/2,
                                    filterList/3,
                                    printLoad/3]).

:- use_module(courseRecommendation,[askMC/1,
                                    filterKeyRecom/3]).

:- use_module(courseInformation,[askCNum/1,
                                    printCourse/1]).

:- use_module(lambda,[lambda/2]).

:- use_module(readLine,[readLine/1,
                        readLineNoDecap/1]).

/*========================================================================
   Features
========================================================================*/

/* Course planning: Give a list of course recommendations */
coursePlanning:-
    askEnroll(UserEnroll),
    filterEnroll(UserEnroll,CourseEnroll),
    askSem(UserSem),
    filterSem(UserSem,CourseEnroll,CourseSem),
    askKey(UserKey),
    filterKey(UserKey,CourseSem,CourseKey),
    askPrev(UserPrev),
    filterPrev(UserPrev,CourseKey,CoursePrev),
    askNum(UserNum),
    filterNum(UserNum,CourseNums),
    askMust(UserMust),
    filterMust(UserMust,CourseNums,CoursePrev,Courses),
    printPlan(Courses).


/* Load approximation: Approximate the overall load of a course plan */
loadApproximation:-
    askEnroll(UserEnroll),
    filterEnrollLoad(UserEnroll,LoadMax),
    askList(UserList),
	filterList(UserList,CourseNums,Load),
    printLoad(CourseNums,Load,LoadMax).


/* Course recommendation: Recommend a course based on user's interest */
courseRecommendation:-
    askEnroll(UserEnroll),
    filterEnroll(UserEnroll,CourseEnroll),
    askSem(UserSem),
    filterSem(UserSem,CourseEnroll,CourseSem),
    askKey(UserKey),
    filterKeyRecom(UserKey,CourseSem,CourseKey),
    askMC(CourseKey).


/* Course information: Get the info of a course */
courseInformation:-
    askCNum(UserCNum),
    printCourse(UserCNum).


/*========================================================================
   Main function helpers
========================================================================*/

/* Ask user's name */
askName(Name):-
    nl,write('Hi there! I will be assisting you today. How can I call you?'),nl,
    write('Please reply to me like "My name is Bryan"'),nl,
    readLineNoDecap(Sentence),!,
    Sentence=[_,_,_|[Name|_]].


/* Ask the task that the user wants to do */
askTask(Name,Task):-
    nl,format('Well, nice to hear from you, ~w! How can I help you today?', [Name]),nl,
    format('Please reply like "I want to ..." and choose from the following:~n  - make a course plan~n  - approximate course load~n  - get a course recommendation~n  - get the info of a course'),nl,
    readLine(Task).


/* Ask if the user wants more help */
askMore:-
    nl,write('Is there anything else that I can assist you with? (yes/no)'),nl,
    readLine(MoreStr),
    (
        MoreStr=[yes],!;
        MoreStr=[y]
    ).


/*========================================================================
   Main function
========================================================================*/

chat2plan:-
    askName(UserName),
    chat2planIter(UserName).


/* chat2plan helper to continuously assist user */
chat2planIter(UserName):-
    askTask(UserName,UserTask),
    (
        lambda(UserTask,Sems),
        (
            Sems=[want(user,coursePlan)],
            coursePlanning,!
        ;
            Sems=[want(user,loadApprox)],
            loadApproximation,!
        ;
            Sems=[want(user,courseRecom)],
            courseRecommendation,!
        ;
            Sems=[want(user,courseInfo)],
            courseInformation,!
        )
    ;
        nl,write('I can\'t understand you. Please try again...'),nl,
        retry(Sems),
        (
            Sems=[want(user,coursePlan)],
            coursePlanning,!
        ;
            Sems=[want(user,loadApprox)],
            loadApproximation,!
        ;
            Sems=[want(user,courseRecom)],
            courseRecommendation,!
        ;
            Sems=[want(user,courseInfo)],
            courseInformation
        )
    ),
    (
        askMore,
        chat2planIter(UserName),!
    ;
        nl,write('It was nice chatting with you today. Good bye!'),nl
    ).


/*========================================================================
   Info
========================================================================*/

info:-
   format('~n> ------------------------------------------------------------ <',[]),
   format('~n> Welcome to Chat2Plan! How can I help you today?              <',[]),
   format('~n>                                                              <',[]),
   format('~n> ?- chat2plan.               - interact with our chatbot      <',[]),
   format('~n> ?- coursePlanning.          - make a course plan             <',[]),
   format('~n> ?- loadApproximation.       - approximate course load        <',[]),
   format('~n> ?- courseRecommendation.    - get a course recommendation    <',[]),
   format('~n> ?- courseInformation.       - get the info of a course       <',[]),
   format('~n> ------------------------------------------------------------ <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.
