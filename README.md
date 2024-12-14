---
title: Chat2Plan
app_file: gradioMain.py
sdk: gradio
sdk_version: 5.8.0
---
# Chat2Plan: Course Planner Chatbot for KAIST SoC Students

```
KAIST Fall 2024 CS579 Computational Linguistics: Term Project
Bryan Nathanael Wijaya (20244078) 
bryannwijaya@kaist.ac.kr
```

## Prerequisites

Install SWI-Prolog following the manual [here](https://www.swi-prolog.org/).

To run Chat2Plan with the Gradio-based chatbot interface, additionally install `pyswip` and `gradio` using pip:
```
pip install pyswip gradio
```

## Inference

### Chat2Plan in SWI-Prolog
To run Chat2Plan directly in the SWI-Prolog terminal, run the following to enable SWI-Prolog,
```
swipl
```

Then load `main.pl` and execute the `chat2plan/0` predicate as follows.
```
?- [main].
?- chat2plan.
```

Follow the prompts to plan your course and use other Chat2Plan features!

### Chat2Plan + Gradio
Alternatively, run Chat2Plan using Gradio by executing the Python file:
```
python gradioMain.py
```

Doing so will print out some output to your terminal. Copy and paste one of the outputted links to your web browser, then press Enter. The links should look something like the following.
```
* Running on local URL:  http://127.0.0.1:7860
* Running on public URL: https://573ff216ca1b225ba6.gradio.live
```

You can then use Chat2Plan in the Gradio chatbot to plan your courses! To initiate the interaction, say anything to the chatbot :-)

## Remarks

Important files for this project (those implemented outside BB1):
- Course knowledge base 
   - `courseKB.pl`: Course knowledge base
   - `preprocess.ipynb`: Course knowledge base preprocessing notebook

- Chat2Plan features and interactions
   - `main.py`: Main predicates for Chat2Plan in SWI-Prolog
   - `coursePlanning.pl`: Helper predicates for feature 1 (course planning)
   - `loadApproximation.pl`: Helper predicates for feature 2 (load approximation)
   - `courseRecommendation.pl`: Helper predicates for feature 3 (course recommendation)
   - `courseInformation.pl`: Helper predicates for feature 4 (course information)
   - `englishLexicon.pl`: Registered lexicons for Chat2Plan lambda calculus-based representations
   
- Integration with Gradio
   - `gradioMain.py`: Main functions for running Chat2Plan with Gradio chatbot
   - `gradioHelper.pl`: Helper functions for Chat2Plan with Gradio due to the restrictions and I/O specifications in `pyswip` and `gradio`
   

