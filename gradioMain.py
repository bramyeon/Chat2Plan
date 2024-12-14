'''
Following the chatbot function format in Gradio,
we implement Chat2Plan for Gradio by considering the system
as a finite-state machine, where user answers determine the 
next state of the system. We have five main ststes with multiple
substates signifying the progress within each state.

- Initialize: 4 sub-states 
- Course planning: 6 sub-states
- Load approximation: 2 sub-states
- Course recommendation: 4 sub-states
- Course information: 1 sub-state

The main states are saved as TASK_CODE: 0, 1, 2, 3, 4
The sub-states are saved as SUBTASK_CODE: 0, 1, 2, 3, 4, 5
'''

import gradio as gr
from pyswip import Prolog
import re

TASK_CODE=0
SUBTASK_CODE=0
NAME=''
COURSE_LIST=[]
COURSE_NUMS=0
LOAD_MAX=0

def parseMessage(message, decap=True):
    def tryInt(elem):
        try:
            return int(elem)
        except:
            return elem
        
    if decap:
        parsed = re.sub(r"[^a-zA-Z0-9 ]", "", message).lower().split()
        return [tryInt(elem) for elem in parsed]
    else:
        parsed = re.sub(r"[^a-zA-Z0-9 ]", "", message).split()
        return [tryInt(elem) for elem in parsed]

def initiate(msg):
    global TASK_CODE
    global SUBTASK_CODE
    global NAME
    global COURSE_LIST
    global COURSE_NUMS
    global LOAD_MAX
    message = parseMessage(msg)
    
    if SUBTASK_CODE==0:
        replyStr = 'Hi there! I will be assisting you today. How can I call you?\nPlease reply to me like "My name is Bryan"'
        SUBTASK_CODE=1
    
    elif SUBTASK_CODE==1:
        message = parseMessage(msg, False)
        NAME = [elem['Name'] for elem in prolog.query(f"askNameGradio({message},Name)")][0]
        replyStr = f'Well, nice to hear from you, {NAME}! How can I help you today?\nPlease reply like "I want to ..." and choose from the following:\n  - make a course plan\n  - approximate course load\n  - get a course recommendation\n  - get the info of a course'
        SUBTASK_CODE=2
        
    elif SUBTASK_CODE==2:
        try:
            action = [elem['Action'] for elem in prolog.query(f"askTaskGradio({message},Action)")][0]
            if action == 'coursePlan':
                replyStr = f'Okay, let\'s begin! Which program are you currently enrolled in? Undergraduate or graduate?\nAn example response would be "I am a graduate student"'
                TASK_CODE=1
                SUBTASK_CODE=0
            elif action == 'loadApprox':
                replyStr = f'Okay, let\'s begin! Which program are you currently enrolled in? Undergraduate or graduate?\nAn example response would be "I am a graduate student"'
                TASK_CODE=2
                SUBTASK_CODE=0
            elif action == 'courseRecom':
                replyStr = f'Okay, let\'s begin! Which program are you currently enrolled in? Undergraduate or graduate?\nAn example response would be "I am a graduate student"'
                TASK_CODE=3
                SUBTASK_CODE=0
            elif action == 'courseInfo':
                replyStr = f'Which course would you like to know about?\nSome example responses would be "Tell me everything about CS579" or "Tell me everything about CS492 Unconventional Computing"'
                TASK_CODE=4
                SUBTASK_CODE=0
        except:
            replyStr = 'I can\'t understand you. Please try again...'
            TASK_CODE=0
            SUBTASK_CODE=2
    
    elif SUBTASK_CODE==3:
        if 'yes' in message or 'y' in message:
            COURSE_LIST=[]
            COURSE_NUMS=0
            LOAD_MAX=0
            replyStr = f'Well, nice to hear from you, {NAME}! How can I help you today?\nPlease reply like "I want to ..." and choose from the following:\n  - make a course plan\n  - approximate course load\n  - get a course recommendation\n  - get the info of a course'
            SUBTASK_CODE=2
        else:
            replyStr = f'It was nice chatting with you today. Good bye!'
            SUBTASK_CODE=0
            
    else:
        return AssertionError
            
    return replyStr

def coursePlanning(msg):
    global TASK_CODE
    global SUBTASK_CODE
    global NAME
    global COURSE_LIST
    global COURSE_NUMS
    message = parseMessage(msg)
    
    # filter by enrollment info
    if SUBTASK_CODE==0:
        try:
            COURSE_LIST = [elem['Courses'] for elem in prolog.query(f"filterEnrollGradio({message},Courses)")][0]
            replyStr = f'Awesome! For which semester are we planning your courses? Spring or fall?\nAn example response would be "I want to plan for the spring semester"'
            SUBTASK_CODE=1
        except:
            replyStr = f'I can\'t understand you. Please try again...'
            SUBTASK_CODE=0
    
    # filter by semester
    elif SUBTASK_CODE==1:
        try:
            COURSE_LIST = [elem['FinCourses'] for elem in prolog.query(f"filterSemGradio({message},{COURSE_LIST},FinCourses)")][0]
            replyStr = f'Now, what topics are you interested in?\nYou can choose from the following list and separate with "and":\n  - AI-Information Service / AI\n  - Algorithms\n  - Computer Architecture\n  - Computing Philosophy\n  - Computing Theory\n  - Data Science\n  - Data Structure\n  - Digital System\n  - Entrepreneurship\n  - Ethics\n  - Interactive Computing\n  - Machine Learning / ML\n  - Natural Language Processing / NLP\n  - Networking\n  - Operating Systems / OS\n  - Programming Basics\n  - Programming Language / PL\n  - Research Methodology\n  - Robotics\n  - Secure Computing\n  - Social Computing\n  - Software Design\n  - Systems-Networks\n  - Technical Writing\n  - Visual Computing / Computer Vision / CV\nAn example response would be "I am interested in Algorithms and NLP and Machine Learning"'
            SUBTASK_CODE=2
        except:
            replyStr = f'I can\'t understand you. Please try again...'
            SUBTASK_CODE=1
    
    # score by keywords     
    elif SUBTASK_CODE==2:
        try:
            COURSE_LIST = [elem['FinCourses'] for elem in prolog.query(f"filterKeyGradio({message},{COURSE_LIST},FinCourses)")][0]
            COURSE_LIST = [(f'{str(elem)}')[2:-1].split(', ') for elem in COURSE_LIST]
            COURSE_LIST = [(str(elem[0]), int(elem[1])) for elem in COURSE_LIST]
            replyStr = f'Perfect! This might be a bit annoying, but, could you let me know all the SoC courses you have previously taken?\nPlease separate the course numbers with "and" and for special topics, please include the course subtitle.\nAn example response would be "I took CS101 and CS376 and CS492 Diffusion Models and Their Applications", or if you haven\'t take any courses from the SoC, simply say "No"'
            SUBTASK_CODE=3
        except:
            replyStr = f'I can\'t understand you. Please try again...'
            SUBTASK_CODE=2
    
    # filter and score by previously taken courses     
    elif SUBTASK_CODE==3:
        try:
            COURSE_LIST = [elem['FinCourses'] for elem in prolog.query(f"filterPrevGradio({message},{COURSE_LIST},FinCourses)")][0]
            COURSE_LIST = [(f'{str(elem)}')[2:-1].split(', ') for elem in COURSE_LIST]
            COURSE_LIST = [(str(elem[0]), int(elem[1])) for elem in COURSE_LIST]
            replyStr = f'We\'re almost there! How many classes do you plan to take next semester?\nAn example response would be "I plan to take 5 courses"'
            SUBTASK_CODE=4
        except:
            replyStr = f'I can\'t understand you. Please try again...'
            SUBTASK_CODE=3
            
    # get the number of courses to take 
    elif SUBTASK_CODE==4:
        try:
            COURSE_NUMS = int([elem['CourseNums'] for elem in prolog.query(f"filterNumGradio({message},CourseNums)")][0])
            replyStr = f'Finally, is there any course that you must take next semester? (yes/no)\nIf yes, separate the course numbers with "and".\nSome example responses would be "Yes, I must take CS330 and CS320" or "No"'
            SUBTASK_CODE=5
        except:
            replyStr = f'I can\'t understand you. Please try again...'
            SUBTASK_CODE=4
    
    # add must-take courses and return the course plan
    elif SUBTASK_CODE==5:
        try:
            courseStr = [str(elem['CourseStr']) for elem in prolog.query(f"filterMustGradio({message},{COURSE_NUMS},{COURSE_LIST},CourseStr)")][0]
            replyStr = f'{courseStr[2:-1]}\n\nIs there anything else that I can assist you with? (yes/no)'
            TASK_CODE=0
            SUBTASK_CODE=3
        except:
            replyStr = f'I can\'t understand you. Please try again...'
            TASK_CODE=1
            SUBTASK_CODE=5
    
    else:
        return AssertionError
    
    return replyStr

def loadApproximation(msg):
    global TASK_CODE
    global SUBTASK_CODE
    global LOAD_MAX
    message = parseMessage(msg)
    
    # filter by enrollment info
    if SUBTASK_CODE==0:
        try:
            LOAD_MAX = int([elem['LoadMax'] for elem in prolog.query(f"filterEnrollLoadGradio({message},LoadMax)")][0])
            replyStr = f'Now, could you let me know all the SoC courses you that plan to take?\nPlease separate the course numbers with "and" and for special topics, please include the course subtitle.\nAn example response would be "I am taking CS101 and CS376 and CS492 Diffusion Models and Their Applications"'
            SUBTASK_CODE=1
        except:
            replyStr = f'I can\'t understand you. Please try again...'
            SUBTASK_CODE=0
    
    # calculate course load    
    elif SUBTASK_CODE==1:
        try:
            courseNums = int([elem['CourseNums'] for elem in prolog.query(f"filterListGradio({message},CourseNums,Load)")][0])
            load = int([elem['Load'] for elem in prolog.query(f"filterListGradio({message},CourseNums,Load)")][0])
            if load > LOAD_MAX:
                replyStr = f'Holy moly! You have {courseNums} classes with a total load score of {load}, where the optimum load for your program is {LOAD_MAX}. Please considering dropping some of them.\n\nIs there anything else that I can assist you with? (yes/no)'
            else:
                replyStr = f'Looking good! You have {courseNums} classes with a total load score of {load}, where the optimum load for your program is {LOAD_MAX}.\n\nIs there anything else that I can assist you with? (yes/no)'
            TASK_CODE=0
            SUBTASK_CODE=3
        except:
            replyStr = f'I can\'t understand you. Please try again...'
            TASK_CODE=2
            SUBTASK_CODE=1
            
    else:
        return AssertionError
    
    return replyStr

def courseRecommendation(msg):
    global TASK_CODE
    global SUBTASK_CODE
    global NAME
    global COURSE_LIST
    message = parseMessage(msg)
    
    # filter by enrollment info
    if SUBTASK_CODE==0:
        try:
            COURSE_LIST = [elem['Courses'] for elem in prolog.query(f"filterEnrollGradio({message},Courses)")][0]
            replyStr = f'Awesome! For which semester are we planning your courses? Spring or fall?\nAn example response would be "I want to plan for the spring semester"'
            SUBTASK_CODE=1
        except:
            replyStr = f'I can\'t understand you. Please try again...'
            SUBTASK_CODE=0
    
    # filter by semester
    elif SUBTASK_CODE==1:
        try:
            COURSE_LIST = [elem['FinCourses'] for elem in prolog.query(f"filterSemGradio({message},{COURSE_LIST},FinCourses)")][0]
            replyStr = f'Now, what topics are you interested in?\nYou can choose from the following list and separate with "and":\n  - AI-Information Service / AI\n  - Algorithms\n  - Computer Architecture\n  - Computing Philosophy\n  - Computing Theory\n  - Data Science\n  - Data Structure\n  - Digital System\n  - Entrepreneurship\n  - Ethics\n  - Interactive Computing\n  - Machine Learning / ML\n  - Natural Language Processing / NLP\n  - Networking\n  - Operating Systems / OS\n  - Programming Basics\n  - Programming Language / PL\n  - Research Methodology\n  - Robotics\n  - Secure Computing\n  - Social Computing\n  - Software Design\n  - Systems-Networks\n  - Technical Writing\n  - Visual Computing / Computer Vision / CV\nAn example response would be "I am interested in Algorithms and NLP and Machine Learning"'
            SUBTASK_CODE=2
        except:
            replyStr = f'I can\'t understand you. Please try again...'
            SUBTASK_CODE=1
    
    # score by keywords and retrieve the head course
    elif SUBTASK_CODE==2:
        try:
            COURSE_LIST = [elem['FinCourses'] for elem in prolog.query(f"filterKeyRecomGradio({message},{COURSE_LIST},FinCourses)")][0]
            courseStr = [str(elem['CourseStr']) for elem in prolog.query(f"retrieveCourseGradio({COURSE_LIST[0]},CourseStr)")][0]
            replyStr = f'{courseStr[2:-1]}'
            COURSE_LIST = COURSE_LIST[1:]
            SUBTASK_CODE=3
        except:
            replyStr = f'I can\'t understand you. Please try again...'
            SUBTASK_CODE=2
            
    # retrieve the next head course course
    elif SUBTASK_CODE==3:
        try:
            if 'yes' in message or 'y' in message:
                if len(COURSE_LIST) > 0:
                    courseStr = [str(elem['CourseStr']) for elem in prolog.query(f"retrieveCourseGradio({COURSE_LIST[0]},CourseStr)")][0]
                    replyStr = f'{courseStr[2:-1]}'
                    COURSE_LIST = COURSE_LIST[1:]
                    TASK_CODE=3
                    SUBTASK_CODE=3
                else:
                    replyStr = f'Sorry! I ran out of recommendations. Maybe try more keywords?\n\nIs there anything else that I can assist you with? (yes/no)'
                    TASK_CODE=0
                    SUBTASK_CODE=3
            elif 'no' in message or 'n' in message:
                replyStr = f'Is there anything else that I can assist you with? (yes/no)'
                TASK_CODE=0
                SUBTASK_CODE=3
                
        except:
            replyStr = f'I can\'t understand you. Please try again...'
            TASK_CODE=3
            SUBTASK_CODE=3
    
    else:
        return AssertionError
    
    return replyStr

def courseInformation(msg):
    global TASK_CODE
    global SUBTASK_CODE
    message = parseMessage(msg)
    
    # get the info of a course
    if SUBTASK_CODE==0:
        courseNum = [str(elem['CourseNum']) for elem in prolog.query(f"askCNumGradio({message},CourseNum)")][0]
        try:
            courseStr = [str(elem['CourseStr']) for elem in prolog.query(f"printCourseGradio('{str(courseNum)}',CourseStr)")][0]
            replyStr = f'{courseStr[2:-1]}\n\nIs there anything else that I can assist you with? (yes/no)'
            TASK_CODE=0
            SUBTASK_CODE=3
        except:
            replyStr = f'I can\'t understand you. Please try again...'
            TASK_CODE=4
            SUBTASK_CODE=0
            
    else:
        return AssertionError
            
    return replyStr
    
def chat2plan(msg, history):
    global TASK_CODE
    global SUBTASK_CODE
    global NAME
    global COURSE_LIST
    global COURSE_NUMS
    global LOAD_MAX
    
    if TASK_CODE == 0:
        return initiate(msg)
    
    elif TASK_CODE == 1:
        return coursePlanning(msg)
    
    elif TASK_CODE == 2:
        return loadApproximation(msg)
    
    elif TASK_CODE == 3:
        return courseRecommendation(msg)
    
    elif TASK_CODE == 4:
        return courseInformation(msg)
    
    else:
        return AssertionError
    

if __name__=='__main__':
    prolog = Prolog()
    prolog.consult('gradioHelper.pl')
    
    gr.ChatInterface(
        chat2plan,
        type="messages",
        chatbot=gr.Chatbot(height=500),
        textbox=gr.Textbox(placeholder="Ask me a yes or no question", container=False, scale=7),
        title="Chat2Plan",
        description="Chat2Plan: Course Planner Chatbot for KAIST SoC Students",
        theme="ocean",
        # examples=examples,
        # cache_examples=True,
    ).launch(share=True)
    
    # while True:
    #     msg = input()
    #     reply = chat2plan(msg,None)
    #     print()
    #     print(reply)
    #     print()