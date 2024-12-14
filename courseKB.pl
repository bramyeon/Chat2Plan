/*================================================================================

KAIST SoC Course Knowledge Base for CS579 Term Project (Fall 2024)
Project Title: 'Chat2Plan: Course Planner Chatbot for KAIST SoC Students'
Compiled by Bryan Nathanael Wijaya (20244078)

================================================================================*/


/* The courses are represented following the format below.
   course(courseNumSub, courseTitle, courseSubtitle, courseType, mutualRecognition, openingSemester, regularOpening, requiredPrerequisites, recommendedPrerequisites, keywords, courseLoad).
*/

:- module(courseKB,[course/11]).

course('cs101','Introduction to Programming','nan','Basic Required',false,'spring and fall',true,[],[],['Computing Theory', 'Programming Basics'],1).
course('cs109','Programming Practice','nan','Basic Elective',false,'spring and fall',true,[],['CS101'],['Computing Theory', 'Programming Basics', 'Data Structure'],2).
course('cs202','Problem Solving','nan','Major Elective',false,'spring and fall',false,[],[],['Computing Theory', 'Algorithms'],1).
course('cs204','Discrete Mathematics','nan','Major Required',false,'spring and fall',true,[],[],['Computing Theory', 'Software Design', 'Secure Computing'],2).
course('cs206','Data Structure','nan','Major Required',false,'spring and fall',true,[],[],['Computing Theory', 'Data Science', 'Data Structure'],3).
course('cs211','Digital System and Lab','nan','Major Elective',false,'spring',false,[],[],['Systems-Networks', 'Digital System'],3).
course('cs220','Programming Principles','nan','Major Elective',false,'spring and fall',true,[],[],['Computing Theory', 'Programming Basics'],2).
course('cs230','System Programming','nan','Major Elective',false,'spring and fall',true,[],[],['Systems-Networks', 'Secure Computing', 'Computer Architecture'],3).
course('cs270','Intelligent robot design and programming','nan','Major Elective',false,'spring',true,[],[],['AI-Information Service', 'Interactive Computing', 'Machine Learning', 'Robotics'],4).
course('cs300','Introduction to Algorithms','nan','Major Required',false,'spring and fall',true,['CS204', 'CS206'],['CS204', 'CS206'],['Computing Theory', 'Algorithms'],2).
course('cs311','Computer Organization','nan','Major Required',false,'spring and fall',true,['CS211'],['CS211', 'CS230'],['Systems-Networks', 'Computer Architecture'],4).
course('cs320','Programming Language','nan','Major Required',false,'spring and fall',true,['CS206'],['CS204', 'CS230'],['Computing Theory', 'Programming Language'],5).
course('cs322','Formal Languages and Automata','nan','Major Elective',false,'fall',false,['CS204'],[],['Computing Theory', 'Natural Language Processing'],3).
course('cs330','Operating Systems and Lab','nan','Major Required',false,'spring and fall',true,[],['CS230'],['Systems-Networks', 'Secure Computing', 'Operating Systems'],5).
course('cs341','Introduction to Computer Networks','nan','Major Elective',false,'spring and fall',true,['CS230'],['CS230'],['Systems-Networks', 'Secure Computing', 'Networking'],3).
course('cs348','Introduction to Information Security','nan','Major Elective',false,'spring',true,['CS220', 'CS230'],['CS220', 'CS230'],['Secure Computing'],2).
course('cs350','Introduction to Software Engineering','nan','Major Elective',false,'spring and fall',true,[],[],['Software Design'],2).
course('cs360','Introduction to Database','nan','Major Elective',false,'spring and fall',true,['CS206'],['CS206'],['Data Science', 'Social Computing'],3).
course('cs361','Introduction to Data Science','nan','Major Elective',false,'spring',false,[],['CS206'],['Data Science', 'Machine Learning'],2).
course('cs371','Introduction to Deep Learning','nan','Major Elective',false,'fall',false,[],[],['AI-Information Service', 'Machine Learning', 'Visual Computing'],2).
course('cs372','Natural Language Processing with Python','nan','Major Elective',false,'spring',true,[],[],['AI-Information Service', 'Social Computing', 'Natural Language Processing'],3).
course('cs374','Introduction to Human-Computer Interaction','nan','Major Elective',false,'spring and fall',true,[],[],['Social Computing', 'Interactive Computing'],2).
course('cs376','Machine Learning','nan','Major Elective',false,'spring and fall',true,[],[],['Visual Computing', 'AI-Information Service', 'Machine Learning'],2).
course('cs380','Introduction to Computer Graphics','nan','Major Elective',false,'spring',true,[],[],['Visual Computing', 'Interactive Computing'],2).
course('cs402','Introduction to Logic for Computer Science','nan','Major Elective',true,'spring',false,[],['CS300', 'CS320'],['Computing Theory', 'Natural Language Processing'],2).
course('cs408','Computer Science Project','nan','Major Elective',true,'spring and fall',false,[],[],['Software Design'],3).
course('cs409','Software Projects for Industrial Collaboration','nan','Major Elective',true,'spring and fall',false,[],[],['Software Design'],4).
course('cs411','System for Artificial Intelligence','nan','Major Elective',true,'fall',false,['CS230', 'CS311'],[],['Systems-Networks', 'AI-Information Service', 'Computer Architecture'],5).
course('cs420','Compiler Design','nan','Major Elective',true,'spring and fall',true,[],['CS300'],['Secure Computing', 'Programming Language'],4).
course('cs422','Computation Theory','nan','Major Elective',true,'spring and fall',false,[],['CS300'],['Computing Theory', 'Systems-Networks'],5).
course('cs423','Probabilistic Programming','nan','Major Elective',true,'spring',false,['CS376', 'CS320'],['CS300', 'CS320', 'CS376'],['AI-Information Service', 'Machine Learning', 'Algorithms'],4).
course('cs424','Program Reasoning','nan','Major Elective',true,'fall',false,[],[],['Computing Theory'],3).
course('cs431','Concurrent Programming','nan','Major Elective',true,'spring and fall',true,[],[],['Computing Theory'],4).
course('cs442','Mobile Computing and Applications','nan','Major Elective',true,'spring and fall',false,[],[],['Social Computing', 'Interactive Computing', 'Systems-Networks'],3).
course('cs443','Distributed Algorithms and Systems','nan','Major Elective',true,'fall',false,['CS330', 'CS341'],[],['Systems-Networks', 'Algorithms', 'Computer Architecture'],4).
course('cs447','Web Security Attack Laboratary','nan','Major Elective',true,'spring',false,[],[],['Secure Computing'],3).
course('cs448','Introduction to Information Security','nan','Major Elective',true,'spring',false,[],[],['Secure Computing'],2).
course('cs453','Automated Software Testing','nan','Major Elective',true,'spring and fall',false,[],['CS350'],['Software Design'],3).
course('cs454','Artificial Intelligence Based Software Engineering','nan','Major Elective',true,'fall',false,[],['CS350'],['Software Design', 'Machine Learning'],4).
course('cs457','Requirements Engineering for Smart Environments','nan','Major Elective',true,'spring',true,[],['CS350'],['Software Design', 'Interactive Computing'],3).
course('cs458','Dynamic Analysis of Software Source Code','nan','Major Elective',true,'spring and fall',true,[],[],['Computing Theory'],4).
course('cs459','Introduction to Services Computing','nan','Major Elective',true,'fall',false,[],['CS330'],['Computing Theory'],2).
course('cs470','Introduction to Artificial Intelligence','nan','Major Elective',true,'spring and fall',true,[],['CS372', 'CS376'],['AI-Information Service', 'Social Computing', 'Machine Learning'],3).
course('cs471','Graph Machine Learning and Mining','nan','Major Elective',true,'spring',false,['CS376'],[],['AI-Information Service', 'Data Science', 'Machine Learning'],3).
course('cs473','Introduction to Social Computing','nan','Major Elective',true,'spring and fall',false,[],['CS374'],['Social Computing', 'Interactive Computing'],3).
course('cs474','Text Mining','nan','Major Elective',true,'spring and fall',false,[],['CS372', 'CS376'],['AI-Information Service', 'Natural Language Processing'],2).
course('cs475','Machine Learning for Natural Language Processing','nan','Major Elective',true,'spring and fall',true,[],['CS372', 'CS376'],['Computing Theory', 'Natural Language Processing', 'Machine Learning'],4).
course('cs477','Introduction to Intelligent Robotics','nan','Major Elective',true,'spring',false,[],[],['Computing Theory', 'Robotics', 'Machine Learning', 'AI-Information Service'],2).
course('cs479','Machine Learning for 3D Data','nan','Major Elective',true,'fall',false,[],[],['Computing Theory', 'Visual Computing', 'Machine Learning'],5).
course('cs481','Data Visualization','nan','Major Elective',true,'spring',false,[],[],['Computing Theory', 'Visual Computing'],3).
course('cs482','Interactive Computer Graphics','nan','Major Elective',true,'fall',true,[],[],['Visual Computing', 'Interactive Computing'],2).
course('cs484','Introduction to Computer Vision','nan','Major Elective',true,'fall',true,[],['CS470'],['Visual Computing', 'AI-Information Service'],4).
course('cs485','Machine Learning for Computer Vision','nan','Major Elective',true,'fall',false,[],[],['Visual Computing', 'AI-Information Service', 'Machine Learning'],3).
course('cs486','Wearable User Interface','nan','Major Elective',true,'spring',false,[],[],['Interactive Computing'],2).
course('cs489','Computer Ethics & Social issues','nan','Major Elective',true,'fall',false,[],[],['Social Computing', 'Computing Philosophy', 'Ethics'],1).
course('cs492 futures of the world','Special Topics in Computer Science','Futures of the World','Major Elective',true,'fall',false,[],[],['Computing Philosophy'],1).
course('cs492 ai prototyping','Special Topics in Computer Science','AI Prototyping','Major Elective',true,'fall',false,[],[],['AI-Information Service', 'Machine Learning'],4).
course('cs492 ai for smart life','Special Topics in Computer Science','AI for Smart Life','Major Elective',true,'fall',false,[],[],['AI-Information Service', 'Machine Learning'],3).
course('cs492 ai industry and smart energy','Special Topics in Computer Science','AI industry and smart energy','Major Elective',true,'spring',false,[],[],['AI-Information Service', 'Machine Learning'],3).
course('cs492 algorithms design and analysis for nphard problems','Special Topics in Computer Science','Algorithms Design and Analysis for NP-Hard Problems','Major Elective',true,'spring',false,[],[],['Data Science', 'Computing Theory', 'Algorithms'],5).
course('cs492 android app development for the internet business service model using the deepfake technology  from design to registration and optimization','Special Topics in Computer Science','Android App Development for the Internet Business Service Model using the Deepfake Technology : From Design to Registration and Optimization','Major Elective',true,'fall',false,[],[],['AI-Information Service', 'Software Design'],4).
course('cs492 another computer science','Special Topics in Computer Science','Another Computer Science','Major Elective',true,'spring',false,[],[],['Computing Theory', 'Computing Philosophy'],2).
course('cs492 building blockchain and web3 applications','Special Topics in Computer Science','Building Blockchain and Web3 Applications','Major Elective',true,'spring',false,[],[],['Systems-Networks', 'Software Design'],4).
course('cs492 computational learning theory','Special Topics in Computer Science','Computational Learning Theory','Major Elective',true,'fall',false,[],[],['AI-Information Service', 'Computing Theory'],4).
course('cs492 cyber warfare','Special Topics in Computer Science','Cyber Warfare','Major Elective',true,'spring',false,[],[],['Secure Computing', 'Systems-Networks'],4).
course('cs492 data visualization','Special Topics in Computer Science','Data Visualization','Major Elective',true,'fall',false,[],[],['Visual Computing', 'Interactive Computing'],3).
course('cs492 deep learning for realworld problems','Special Topics in Computer Science','Deep Learning for Real-World Problems','Major Elective',true,'fall',false,[],[],['AI-Information Service', 'Machine Learning'],4).
course('cs492 deep reinforcement learning and game ai','Special Topics in Computer Science','Deep Reinforcement Learning and Game AI','Major Elective',true,'spring',false,[],[],['AI-Information Service', 'Machine Learning'],5).
course('cs492 deep reinforcement learning','Special Topics in Computer Science','Deep Reinforcement Learning','Major Elective',true,'spring',false,[],[],['AI-Information Service', 'Machine Learning'],5).
course('cs492 design and analysis of concurrent programs','Special Topics in Computer Science','Design and Analysis of Concurrent Programs','Major Elective',true,'fall',false,[],[],['Computing Theory', 'Systems-Networks'],4).
course('cs492 designing iotbased healthcare services from services to platforms and devices','Special Topics in Computer Science','Designing IoT-based Health-care Services: From Services to Platforms and Devices','Major Elective',true,'spring',false,[],[],['Systems-Networks', 'AI-Information Service', 'Interactive Computing'],4).
course('cs492 diffusion models and their applications','Special Topics in Computer Science','Diffusion Models and Their Applications','Major Elective',true,'fall',false,[],[],['AI-Information Service', 'Visual Computing', 'Machine Learning'],5).
course('cs492 distributed transactions and blockchain','Special Topics in Computer Science','Distributed transactions and blockchain','Major Elective',true,'fall',false,[],[],['Systems-Networks'],4).
course('cs492 formal sw modeling and verification','Special Topics in Computer Science','Formal SW Modeling and Verification','Major Elective',true,'fall',false,[],[],['Software Design'],5).
course('cs492 foundations of ai vr mmorpg','Special Topics in Computer Science','Foundations of AI, VR, MMORPG','Major Elective',true,'fall',false,[],[],['AI-Information Service', 'Visual Computing', 'Machine Learning'],4).
course('cs492 front end development','Special Topics in Computer Science','Front End Development','Major Elective',true,'fall',false,[],[],['Software Design'],2).
course('cs492 generative ai for society','Special Topics in Computer Science','Generative AI for society','Major Elective',true,'fall',false,[],[],['AI-Information Service', 'Social Computing', 'Machine Learning'],2).
course('cs492 geometric computing','Special Topics in Computer Science','Geometric Computing','Major Elective',true,'fall',false,[],[],['Visual Computing', 'Computing Theory'],4).
course('cs492 geometric modeling and processing','Special Topics in Computer Science','Geometric Modeling and Processing','Major Elective',true,'fall',false,[],[],['Visual Computing', 'Computing Theory'],4).
course('cs492 graph machine learning and mining','Special Topics in Computer Science','Graph Machine Learning and Mining','Major Elective',true,'spring',false,['CS376'],[],['AI-Information Service', 'Data Science', 'Machine Learning'],3).
course('cs492 humanai interaction','Special Topics in Computer Science','Human-AI Interaction','Major Elective',true,'spring and fall',false,[],[],['Interactive Computing', 'Social Computing'],3).
course('cs492 introduction to data science','Special Topics in Computer Science','Introduction to Data Science','Major Elective',true,'spring',false,[],[],['Data Science', 'Machine Learning'],2).
course('cs492 introduction to deep learning','Special Topics in Computer Science','Introduction to Deep Learning','Major Elective',true,'fall',false,[],[],['AI-Information Service', 'Machine Learning', 'Visual Computing'],2).
course('cs492 introduction to intelligent robotics','Special Topics in Computer Science','Introduction to Intelligent Robotics','Major Elective',true,'fall',false,[],[],['AI-Information Service', 'Computing Theory', 'Machine Learning', 'Robotics'],2).
course('cs492 introduction to r for data science','Special Topics in Computer Science','Introduction to R for Data Science','Major Elective',true,'spring and fall',false,[],[],['Data Science'],2).
course('cs492 introduction to research','Special Topics in Computer Science','Introduction to Research','Major Elective',true,'spring and fall',false,[],[],['Research Methodology'],2).
course('cs492 introduction to software security','Special Topics in Computer Science','Introduction to Software Security','Major Elective',true,'spring',false,[],[],['Secure Computing'],3).
course('cs492 machine learning for 3d data','Special Topics in Computer Science','Machine Learning for 3D Data','Major Elective',true,'spring',false,[],[],['Computing Theory', 'Visual Computing', 'Machine Learning'],5).
course('cs492 machine learning for computer vision','Special Topics in Computer Science','Machine Learning for Computer Vision','Major Elective',true,'fall',false,[],[],['Visual Computing', 'AI-Information Service', 'Machine Learning'],3).
course('cs492 microarchitecture design','Special Topics in Computer Science','Microarchitecture Design','Major Elective',true,'fall',false,[],[],['Systems-Networks', 'Computer Architecture'],4).
course('cs492 operation of information protection systems and cyber threat management','Special Topics in Computer Science','Operation of Information Protection Systems and Cyber Threat Management','Major Elective',true,'fall',false,[],[],['Secure Computing'],4).
course('cs492 parallel computing','Special Topics in Computer Science','Parallel Computing','Major Elective',true,'spring and fall',false,[],[],['Systems-Networks', 'Computer Architecture'],4).
course('cs492 pervasive and physical ai computing','Special Topics in Computer Science','Pervasive and Physical AI Computing','Major Elective',true,'fall',false,[],[],['AI-Information Service', 'Machine Learning'],4).
course('cs492 physical user interfaces','Special Topics in Computer Science','Physical User Interfaces','Major Elective',true,'spring',false,[],[],['Interactive Computing'],3).
course('cs492 program reasoning','Special Topics in Computer Science','Program Reasoning','Major Elective',true,'fall',false,[],[],['Computing Theory'],3).
course('cs492 service platform development for internet business','Special Topics in Computer Science','Service Platform Development for Internet Business','Major Elective',true,'spring',false,[],[],['AI-Information Service', 'Software Design'],3).
course('cs492 smart energy and environmental technologies and innovative governance','Special Topics in Computer Science','Smart Energy and Environmental Technologies and Innovative Governance','Major Elective',true,'fall',false,[],[],['AI-Information Service', 'Social Computing'],3).
course('cs492 smart factory for humanmachine collaboration','Special Topics in Computer Science','Smart Factory for Human-Machine Collaboration','Major Elective',true,'fall',false,[],[],['AI-Information Service', 'Interactive Computing'],3).
course('cs492 smart health  datadriven service design for health and wellbeing','Special Topics in Computer Science','Smart Health - Data-Driven Service Design for Health and Wellbeing','Major Elective',true,'fall',false,[],[],['AI-Information Service', 'Software Design', 'Data Science'],3).
course('cs492 smart mobility design for designer engineer and data scientist','Special Topics in Computer Science','Smart Mobility Design for Designer, Engineer, and Data Scientist','Major Elective',true,'fall',false,[],[],['AI-Information Service', 'Software Design', 'Data Science'],3).
course('cs492 smart mobility for sustainable society','Special Topics in Computer Science','Smart Mobility for Sustainable Society','Major Elective',true,'spring',false,[],[],['AI-Information Service'],3).
course('cs492 smart technology ai and its social impact','Special Topics in Computer Science','Smart Technology: AI and Its Social Impact','Major Elective',true,'spring',false,[],[],['Social Computing', 'Computing Philosophy'],3).
course('cs492 smart workspace for creative collaboration','Special Topics in Computer Science','Smart Workspace for Creative Collaboration','Major Elective',true,'spring',false,[],[],['Interactive Computing'],3).
course('cs492 smart working space for creative collaboration','Special Topics in Computer Science','Smart working space for creative collaboration','Major Elective',true,'spring',false,[],[],['Interactive Computing'],3).
course('cs492 another side of computer science critique and framing','Special Topics in Computer Science','Another Side of Computer Science: Critique and Framing','Major Elective',true,'spring',false,[],[],['Computing Philosophy'],2).
course('cs492 system for artificial intelligence','Special Topics in Computer Science','System for Artificial Intelligence','Major Elective',true,'spring',false,[],[],['Systems-Networks', 'AI-Information Service', 'Computer Architecture'],5).
course('cs492 systems for machine learning','Special Topics in Computer Science','Systems for Machine Learning','Major Elective',true,'spring',false,[],[],['Systems-Networks', 'AI-Information Service', 'Computer Architecture'],4).
course('cs492 tech for impact','Special Topics in Computer Science','TECH FOR IMPACT','Major Elective',true,'fall',false,[],[],['AI-Information Service', 'Entrepreneurship'],3).
course('cs492 technology for impact','Special Topics in Computer Science','Technology for Impact','Major Elective',true,'fall',false,[],[],['AI-Information Service', 'Entrepreneurship'],3).
course('cs492 trust and safety engineering and practice','Special Topics in Computer Science','Trust and Safety Engineering and Practice','Major Elective',true,'fall',false,[],[],['Secure Computing', 'Ethics'],3).
course('cs492 unconventional computing','Special Topics in Computer Science','Unconventional Computing','Major Elective',true,'fall',false,[],[],['Computing Theory'],4).
course('cs492 virtualization','Special Topics in Computer Science','Virtualization','Major Elective',true,'spring',false,[],[],['Systems-Networks', 'Operating Systems'],5).
course('cs492 wearable interface','Special Topics in Computer Science','Wearable Interface','Major Elective',true,'spring',false,[],[],['Interactive Computing'],3).
course('cs492 web security attack laboratory','Special Topics in Computer Science','Web Security Attack Laboratory','Major Elective',true,'spring and fall',false,[],[],['Secure Computing'],3).
course('cs493 cs for all','Special Topics in Computer Science I','CS for All','Major Elective',false,'spring',false,[],[],['Computing Philosophy', 'Entrepreneurship'],1).
course('cs500','Design and Analysis of Algorithm','nan','Elective(Graduate)',true,'spring',true,[],['CS300'],['Computing Theory', 'Data Science', 'Data Structure', 'Algorithms'],5).
course('cs504','Computational Geometry','nan','Elective(Graduate)',true,'spring and fall',false,[],['CS300'],['Computing Theory', 'Visual Computing', 'Algorithms'],5).
course('cs510','Computer Architecture','nan','Elective(Graduate)',true,'spring',true,[],['CS311'],['Systems-Networks', 'Computer Architecture'],5).
course('cs520','Theory of Programming Language','nan','Elective(Graduate)',true,'spring and fall',false,['CS320'],['CS320'],['Computing Theory', 'Programming Language'],5).
course('cs524','Program Analysis','nan','Elective(Graduate)',true,'spring and fall',false,[],[],['Computing Theory'],5).
course('cs530','Operating System','nan','Elective(Graduate)',true,'spring and fall',false,[],['CS330'],['Systems-Networks', 'Operating Systems'],4).
course('cs540','Network Architecture','nan','Elective(Graduate)',true,'fall',false,[],[],['Systems-Networks', 'Networking'],4).
course('cs543','Distributed Systems','nan','Elective(Graduate)',true,'spring and fall',false,['CS510', 'CS530'],['CS510', 'CS530'],['Systems-Networks', 'Computer Architecture', 'Operating Systems', 'Networking'],5).
course('cs546','Wireless Mobile Internet','nan','Elective(Graduate)',true,'spring and fall',false,[],[],['Systems-Networks', 'Networking'],5).
course('cs548','Advanced Information Security','nan','Elective(Graduate)',true,'fall',false,[],['CS348'],['Secure Computing'],5).
course('cs550','Software Engineering','nan','Elective(Graduate)',true,'spring',false,[],[],['Software Design'],5).
course('cs560','Database System','nan','Elective(Graduate)',true,'spring and fall',false,[],[],['Systems-Networks', 'Social Computing'],4).
course('cs564','Data Science Methodology','nan','Elective(Graduate)',true,'spring and fall',false,[],[],['Data Science', 'Machine Learning'],3).
course('cs565','IoT Data Science','nan','Elective(Graduate)',true,'spring',false,[],[],['Data Science', 'Interactive Computing', 'Machine Learning'],3).
course('cs570','Artificial Intelligence and Machine Learning','nan','Elective(Graduate)',true,'spring and fall',true,[],[],['AI-Information Service', 'Visual Computing', 'Machine Learning'],3).
course('cs572','Intelligent Robotics','nan','Elective(Graduate)',true,'fall',true,[],[],['AI-Information Service', 'Interactive Computing', 'Robotics', 'Machine Learning'],3).
course('cs575','AI Ethics','nan','Elective(Graduate)',true,'spring',true,[],[],['Social Computing', 'Computing Philosophy', 'Ethics'],3).
course('cs576','Computer Vision','nan','Elective(Graduate)',true,'spring',true,[],[],['AI-Information Service', 'Visual Computing', 'Machine Learning'],4).
course('cs577','Robot Learning and Interaction','nan','Elective(Graduate)',true,'fall',false,[],[],['AI-Information Service', 'Interactive Computing', 'Machine Learning', 'Robotics'],4).
course('cs579','Computational Linguistics','nan','Elective(Graduate)',true,'fall',true,[],[],['Computing Theory', 'Natural Language Processing'],4).
course('cs580','Computer Graphics','nan','Elective(Graduate)',true,'spring',true,[],[],['Visual Computing', 'Interactive Computing'],4).
course('cs584','Human-Computer Interaction','nan','Elective(Graduate)',true,'spring and fall',true,[],[],['Interactive Computing', 'Social Computing'],5).
course('cs588','Deep Learning based Image Search','nan','Elective(Graduate)',true,'spring',false,[],[],['AI-Information Service', 'Visual Computing', 'Machine Learning'],4).
course('cs592 advanced humancomputer interaction','Special Topics in Computing','Advanced Human-Computer Interaction','Elective(Graduate)',true,'spring',false,[],[],['Interactive Computing', 'Social Computing'],5).
course('cs592 practical logic','Special Topics in Computing','Practical Logic','Elective(Graduate)',true,'fall',false,[],[],['Computing Theory', 'Natural Language Processing'],3).
course('cs592 robot learning and interaction','Special Topics in Computing','Robot Learning and Interaction','Elective(Graduate)',true,'spring',false,[],[],['AI-Information Service', 'Interactive Computing', 'Robotics', 'Machine Learning'],4).
course('cs592 sensor data science','Special Topics in Computing','Sensor Data Science','Elective(Graduate)',true,'spring',false,[],[],['Data Science', 'Interactive Computing'],3).
course('cs592 introduction to program analysis','Special Topics in Computing','Introduction to Program Analysis','Elective(Graduate)',true,'fall',false,[],[],['Computing Theory'],3).
course('cs610','Parallel Processing','nan','Elective(Graduate)',false,'spring and fall',false,[],[],['Systems-Networks', 'Computer Architecture'],5).
course('cs612','Social network-aware ubiquitous computing','nan','Elective(Graduate)',false,'spring and fall',false,[],[],['Social Computing', 'Computing Theory'],4).
course('cs632','Embedded Operating Systems','nan','Elective(Graduate)',false,'fall',true,[],[],['Systems-Networks', 'Operating Systems'],5).
course('cs662','Distributed Database','nan','Elective(Graduate)',false,'spring and fall',false,[],['CS360', 'CS376', 'CS361'],['Systems-Networks', 'Social Computing', 'Machine Learning'],4).
course('cs665','Advanced Data Mining','nan','Elective(Graduate)',false,'spring and fall',false,[],[],['AI-Information Service', 'Data Science', 'Machine Learning'],4).
course('cs671','Advanced Machine Learning','nan','Elective(Graduate)',false,'spring and fall',false,[],[],['Visual Computing', 'AI-Information Service', 'Machine Learning'],5).
course('cs672','Reinforcement Learning','nan','Elective(Graduate)',false,'spring and fall',false,[],['CS376'],['AI-Information Service', 'Interactive Computing', 'Machine Learning'],5).
course('cs686','Motion Planning and Applications','nan','Elective(Graduate)',false,'spring and fall',false,[],[],['AI-Information Service', 'Machine Learning'],4).
course('cs688','Large-Scale Image & Video Retrieval','nan','Elective(Graduate)',false,'spring and fall',false,[],[],['Visual Computing'],4).
course('cs700 computer science of numerics','Special Topics in Computation Theory','Computer Science of Numerics','Elective(Graduate)',false,'fall',false,[],[],['Computing Theory'],3).
course('cs730','Special Topics in Operating System','nan','Elective(Graduate)',false,'fall',false,[],[],['Systems-Networks', 'Operating Systems'],5).
course('cs770 artificial intelligent deepfake detection','Topics in Computer Vision','Artificial Intelligent Deepfake Detection','Elective(Graduate)',false,'spring',false,[],[],['Visual Computing', 'AI-Information Service', 'Machine Learning'],4).
course('cs772 korean corpus and semantic web','Topics in Natural Language Processing','Korean Corpus and Semantic Web','Elective(Graduate)',false,'fall',false,[],[],['AI-Information Service', 'Natural Language Processing'],4).
course('cs774 ai and ethics','Topics in Artificial Intelligence','AI and Ethics','Elective(Graduate)',false,'spring and fall',false,[],[],['Social Computing', 'Computing Philosophy', 'Ethics'],3).
course('cs790','Technical Writing for Computer Science','nan','Elective(Graduate)',false,'fall',true,[],[],['Technical Writing'],2).
course('cs891 climate tech and startup','Special Topics in Computer Science I','Climate Tech. and Startup','Elective(Graduate)',false,'fall',false,[],[],['AI-Information Service', 'Entrepreneurship'],3).
course('cs893 deep learning design theory','Special Topics in Computer Science III','Deep Learning Design Theory','Elective(Graduate)',false,'fall',false,[],[],['AI-Information Service', 'Machine Learning', 'Computing Theory'],4).
course('cs893 startup design','Special Topics in Computer Science III','Start-up design','Elective(Graduate)',false,'spring and fall',false,[],[],['Entrepreneurship'],4).
