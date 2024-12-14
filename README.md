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

Doing so will print out some output to your terminal. Copy and paste one of the outputted links, which should look something like the following.
```
* Running on local URL:  http://127.0.0.1:7860
* Running on public URL: https://573ff216ca1b225ba6.gradio.live
```

You can then use Chat2Plan to plan your courses! To initiate the interaction, say anything to the chatbot :-)