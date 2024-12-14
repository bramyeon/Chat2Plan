/*************************************************************************

    Modified from: englishLexicon.pl
    By: Bryan Nathanael Wijaya (20244078)
    
    Copyright (C) 2004,2005,2006 Patrick Blackburn & Johan Bos

    This file is part of BB1, version 1.3 (November 2006).

    BB1 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB1 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB1; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

/*========================================================================
   Determiners
========================================================================*/

lexEntry(det,[syntax:[a],mood:decl,type:indef]).
lexEntry(det,[syntax:[an],mood:decl,type:indef]).
lexEntry(det,[syntax:[the],mood:decl,type:def]).

/*========================================================================
   Nouns: Enrollment status
========================================================================*/

lexEntry(noun,[symbol:undergraduate,syntax:[undergraduate,student]]).
lexEntry(noun,[symbol:undergraduate,syntax:[undergraduate]]).
lexEntry(noun,[symbol:undergraduate,syntax:[undergrad,student]]).
lexEntry(noun,[symbol:undergraduate,syntax:[undergrad]]).

lexEntry(noun,[symbol:graduate,syntax:[graduate,student]]).
lexEntry(noun,[symbol:graduate,syntax:[grad,student]]).

/*========================================================================
   Proper Names: User
========================================================================*/

lexEntry(pn,[symbol:user,syntax:[i]]).
lexEntry(pn,[symbol:user,syntax:[im]]).
lexEntry(pn,[symbol:user,syntax:[yes,i]]).
lexEntry(pn,[symbol:user,syntax:[yes,im]]).

/*========================================================================
   Proper Names: Feature actions
========================================================================*/

lexEntry(pn,[symbol:coursePlan,syntax:[make,a,course,plan]]).
lexEntry(pn,[symbol:loadApprox,syntax:[approximate,course,load]]).
lexEntry(pn,[symbol:courseRecom,syntax:[get,a,course,recommendation]]).
lexEntry(pn,[symbol:courseInfo,syntax:[get,the,info,of,a,course]]).

/*========================================================================
   Proper Names: Semester
========================================================================*/

lexEntry(pn,[symbol:spring,syntax:[spring]]).
lexEntry(pn,[symbol:spring,syntax:[spring,semester]]).
lexEntry(pn,[symbol:fall,syntax:[fall]]).
lexEntry(pn,[symbol:fall,syntax:[fall,semester]]).

/*========================================================================
   Proper Names: Course keywords
========================================================================*/

lexEntry(pn,[symbol:ainfos,syntax:[aiinformation,service]]).
lexEntry(pn,[symbol:ainfos,syntax:[ai]]).
lexEntry(pn,[symbol:algos,syntax:[algorithms]]).
lexEntry(pn,[symbol:archi,syntax:[computer,architecture]]).
lexEntry(pn,[symbol:philo,syntax:[computing,philosophy]]).
lexEntry(pn,[symbol:theory,syntax:[computing,theory]]).
lexEntry(pn,[symbol:datasci,syntax:[data,science]]).
lexEntry(pn,[symbol:datastr,syntax:[data,structure]]).
lexEntry(pn,[symbol:digisys,syntax:[digital,system]]).
lexEntry(pn,[symbol:entrep,syntax:[entrepreneurship]]).
lexEntry(pn,[symbol:ethics,syntax:[ethics]]).
lexEntry(pn,[symbol:intcom,syntax:[interactive,computing]]).
lexEntry(pn,[symbol:ml,syntax:[machine,learning]]).
lexEntry(pn,[symbol:ml,syntax:[ml]]).
lexEntry(pn,[symbol:nlp,syntax:[natural,language,processing]]).
lexEntry(pn,[symbol:nlp,syntax:[nlp]]).
lexEntry(pn,[symbol:network,syntax:[networking]]).
lexEntry(pn,[symbol:os,syntax:[operating,systems]]).
lexEntry(pn,[symbol:os,syntax:[os]]).
lexEntry(pn,[symbol:progbas,syntax:[programming,basics]]).
lexEntry(pn,[symbol:proglang,syntax:[programming,language]]).
lexEntry(pn,[symbol:proglang,syntax:[pl]]).
lexEntry(pn,[symbol:rmethod,syntax:[research,methodology]]).
lexEntry(pn,[symbol:robotics,syntax:[robotics]]).
lexEntry(pn,[symbol:seccom,syntax:[secure,computing]]).
lexEntry(pn,[symbol:soccom,syntax:[social,computing]]).
lexEntry(pn,[symbol:swdes,syntax:[software,design]]).
lexEntry(pn,[symbol:sysnet,syntax:[systemsnetworks]]).
lexEntry(pn,[symbol:writing,syntax:[technical,writing]]).
lexEntry(pn,[symbol:cv,syntax:[computer,vision]]).
lexEntry(pn,[symbol:cv,syntax:[cv]]).
lexEntry(pn,[symbol:cv,syntax:[visual,computing]]).

/*========================================================================
   Proper Names: Course numbers
========================================================================*/

lexEntry(pn,[symbol:'cs101',syntax:[cs101]]).
lexEntry(pn,[symbol:'cs109',syntax:[cs109]]).
lexEntry(pn,[symbol:'cs202',syntax:[cs202]]).
lexEntry(pn,[symbol:'cs204',syntax:[cs204]]).
lexEntry(pn,[symbol:'cs206',syntax:[cs206]]).
lexEntry(pn,[symbol:'cs211',syntax:[cs211]]).
lexEntry(pn,[symbol:'cs220',syntax:[cs220]]).
lexEntry(pn,[symbol:'cs230',syntax:[cs230]]).
lexEntry(pn,[symbol:'cs270',syntax:[cs270]]).
lexEntry(pn,[symbol:'cs300',syntax:[cs300]]).
lexEntry(pn,[symbol:'cs311',syntax:[cs311]]).
lexEntry(pn,[symbol:'cs320',syntax:[cs320]]).
lexEntry(pn,[symbol:'cs322',syntax:[cs322]]).
lexEntry(pn,[symbol:'cs330',syntax:[cs330]]).
lexEntry(pn,[symbol:'cs341',syntax:[cs341]]).
lexEntry(pn,[symbol:'cs348',syntax:[cs348]]).
lexEntry(pn,[symbol:'cs350',syntax:[cs350]]).
lexEntry(pn,[symbol:'cs360',syntax:[cs360]]).
lexEntry(pn,[symbol:'cs361',syntax:[cs361]]).
lexEntry(pn,[symbol:'cs371',syntax:[cs371]]).
lexEntry(pn,[symbol:'cs372',syntax:[cs372]]).
lexEntry(pn,[symbol:'cs374',syntax:[cs374]]).
lexEntry(pn,[symbol:'cs376',syntax:[cs376]]).
lexEntry(pn,[symbol:'cs380',syntax:[cs380]]).
lexEntry(pn,[symbol:'cs402',syntax:[cs402]]).
lexEntry(pn,[symbol:'cs408',syntax:[cs408]]).
lexEntry(pn,[symbol:'cs409',syntax:[cs409]]).
lexEntry(pn,[symbol:'cs411',syntax:[cs411]]).
lexEntry(pn,[symbol:'cs420',syntax:[cs420]]).
lexEntry(pn,[symbol:'cs422',syntax:[cs422]]).
lexEntry(pn,[symbol:'cs423',syntax:[cs423]]).
lexEntry(pn,[symbol:'cs424',syntax:[cs424]]).
lexEntry(pn,[symbol:'cs431',syntax:[cs431]]).
lexEntry(pn,[symbol:'cs442',syntax:[cs442]]).
lexEntry(pn,[symbol:'cs443',syntax:[cs443]]).
lexEntry(pn,[symbol:'cs447',syntax:[cs447]]).
lexEntry(pn,[symbol:'cs448',syntax:[cs448]]).
lexEntry(pn,[symbol:'cs453',syntax:[cs453]]).
lexEntry(pn,[symbol:'cs454',syntax:[cs454]]).
lexEntry(pn,[symbol:'cs457',syntax:[cs457]]).
lexEntry(pn,[symbol:'cs458',syntax:[cs458]]).
lexEntry(pn,[symbol:'cs459',syntax:[cs459]]).
lexEntry(pn,[symbol:'cs470',syntax:[cs470]]).
lexEntry(pn,[symbol:'cs471',syntax:[cs471]]).
lexEntry(pn,[symbol:'cs473',syntax:[cs473]]).
lexEntry(pn,[symbol:'cs474',syntax:[cs474]]).
lexEntry(pn,[symbol:'cs475',syntax:[cs475]]).
lexEntry(pn,[symbol:'cs477',syntax:[cs477]]).
lexEntry(pn,[symbol:'cs479',syntax:[cs479]]).
lexEntry(pn,[symbol:'cs481',syntax:[cs481]]).
lexEntry(pn,[symbol:'cs482',syntax:[cs482]]).
lexEntry(pn,[symbol:'cs484',syntax:[cs484]]).
lexEntry(pn,[symbol:'cs485',syntax:[cs485]]).
lexEntry(pn,[symbol:'cs486',syntax:[cs486]]).
lexEntry(pn,[symbol:'cs489',syntax:[cs489]]).
lexEntry(pn,[symbol:'cs492 futures of the world',syntax:[cs492,futures,of,the,world]]).
lexEntry(pn,[symbol:'cs492 ai prototyping',syntax:[cs492,ai,prototyping]]).
lexEntry(pn,[symbol:'cs492 ai for smart life',syntax:[cs492,ai,for,smart,life]]).
lexEntry(pn,[symbol:'cs492 ai industry and smart energy',syntax:[cs492,ai,industry,and,smart,energy]]).
lexEntry(pn,[symbol:'cs492 algorithms design and analysis for nphard problems',syntax:[cs492,algorithms,design,and,analysis,for,nphard,problems]]).
lexEntry(pn,[symbol:'cs492 android app development for the internet business service model using the deepfake technology  from design to registration and optimization',syntax:[cs492,android,app,development,for,the,internet,business,service,model,using,the,deepfake,technology,from,design,to,registration,and,optimization]]).
lexEntry(pn,[symbol:'cs492 another computer science',syntax:[cs492,another,computer,science]]).
lexEntry(pn,[symbol:'cs492 building blockchain and web3 applications',syntax:[cs492,building,blockchain,and,web3,applications]]).
lexEntry(pn,[symbol:'cs492 computational learning theory',syntax:[cs492,computational,learning,theory]]).
lexEntry(pn,[symbol:'cs492 cyber warfare',syntax:[cs492,cyber,warfare]]).
lexEntry(pn,[symbol:'cs492 data visualization',syntax:[cs492,data,visualization]]).
lexEntry(pn,[symbol:'cs492 deep learning for realworld problems',syntax:[cs492,deep,learning,for,realworld,problems]]).
lexEntry(pn,[symbol:'cs492 deep reinforcement learning and game ai',syntax:[cs492,deep,reinforcement,learning,and,game,ai]]).
lexEntry(pn,[symbol:'cs492 deep reinforcement learning',syntax:[cs492,deep,reinforcement,learning]]).
lexEntry(pn,[symbol:'cs492 design and analysis of concurrent programs',syntax:[cs492,design,and,analysis,of,concurrent,programs]]).
lexEntry(pn,[symbol:'cs492 designing iotbased healthcare services from services to platforms and devices',syntax:[cs492,designing,iotbased,healthcare,services,from,services,to,platforms,and,devices]]).
lexEntry(pn,[symbol:'cs492 diffusion models and their applications',syntax:[cs492,diffusion,models,and,their,applications]]).
lexEntry(pn,[symbol:'cs492 distributed transactions and blockchain',syntax:[cs492,distributed,transactions,and,blockchain]]).
lexEntry(pn,[symbol:'cs492 formal sw modeling and verification',syntax:[cs492,formal,sw,modeling,and,verification]]).
lexEntry(pn,[symbol:'cs492 foundations of ai vr mmorpg',syntax:[cs492,foundations,of,ai,vr,mmorpg]]).
lexEntry(pn,[symbol:'cs492 front end development',syntax:[cs492,front,end,development]]).
lexEntry(pn,[symbol:'cs492 generative ai for society',syntax:[cs492,generative,ai,for,society]]).
lexEntry(pn,[symbol:'cs492 geometric computing',syntax:[cs492,geometric,computing]]).
lexEntry(pn,[symbol:'cs492 geometric modeling and processing',syntax:[cs492,geometric,modeling,and,processing]]).
lexEntry(pn,[symbol:'cs492 graph machine learning and mining',syntax:[cs492,graph,machine,learning,and,mining]]).
lexEntry(pn,[symbol:'cs492 humanai interaction',syntax:[cs492,humanai,interaction]]).
lexEntry(pn,[symbol:'cs492 introduction to data science',syntax:[cs492,introduction,to,data,science]]).
lexEntry(pn,[symbol:'cs492 introduction to deep learning',syntax:[cs492,introduction,to,deep,learning]]).
lexEntry(pn,[symbol:'cs492 introduction to intelligent robotics',syntax:[cs492,introduction,to,intelligent,robotics]]).
lexEntry(pn,[symbol:'cs492 introduction to r for data science',syntax:[cs492,introduction,to,r,for,data,science]]).
lexEntry(pn,[symbol:'cs492 introduction to research',syntax:[cs492,introduction,to,research]]).
lexEntry(pn,[symbol:'cs492 introduction to software security',syntax:[cs492,introduction,to,software,security]]).
lexEntry(pn,[symbol:'cs492 machine learning for 3d data',syntax:[cs492,machine,learning,for,'3d',data]]).
lexEntry(pn,[symbol:'cs492 machine learning for computer vision',syntax:[cs492,machine,learning,for,computer,vision]]).
lexEntry(pn,[symbol:'cs492 microarchitecture design',syntax:[cs492,microarchitecture,design]]).
lexEntry(pn,[symbol:'cs492 operation of information protection systems and cyber threat management',syntax:[cs492,operation,of,information,protection,systems,and,cyber,threat,management]]).
lexEntry(pn,[symbol:'cs492 parallel computing',syntax:[cs492,parallel,computing]]).
lexEntry(pn,[symbol:'cs492 pervasive and physical ai computing',syntax:[cs492,pervasive,and,physical,ai,computing]]).
lexEntry(pn,[symbol:'cs492 physical user interfaces',syntax:[cs492,physical,user,interfaces]]).
lexEntry(pn,[symbol:'cs492 program reasoning',syntax:[cs492,program,reasoning]]).
lexEntry(pn,[symbol:'cs492 service platform development for internet business',syntax:[cs492,service,platform,development,for,internet,business]]).
lexEntry(pn,[symbol:'cs492 smart energy and environmental technologies and innovative governance',syntax:[cs492,smart,energy,and,environmental,technologies,and,innovative,governance]]).
lexEntry(pn,[symbol:'cs492 smart factory for humanmachine collaboration',syntax:[cs492,smart,factory,for,humanmachine,collaboration]]).
lexEntry(pn,[symbol:'cs492 smart health  datadriven service design for health and wellbeing',syntax:[cs492,smart,health,datadriven,service,design,for,health,and,wellbeing]]).
lexEntry(pn,[symbol:'cs492 smart mobility design for designer engineer and data scientist',syntax:[cs492,smart,mobility,design,for,designer,engineer,and,data,scientist]]).
lexEntry(pn,[symbol:'cs492 smart mobility for sustainable society',syntax:[cs492,smart,mobility,for,sustainable,society]]).
lexEntry(pn,[symbol:'cs492 smart technology ai and its social impact',syntax:[cs492,smart,technology,ai,and,its,social,impact]]).
lexEntry(pn,[symbol:'cs492 smart workspace for creative collaboration',syntax:[cs492,smart,workspace,for,creative,collaboration]]).
lexEntry(pn,[symbol:'cs492 smart working space for creative collaboration',syntax:[cs492,smart,working,space,for,creative,collaboration]]).
lexEntry(pn,[symbol:'cs492 another side of computer science critique and framing',syntax:[cs492,another,side,of,computer,science,critique,and,framing]]).
lexEntry(pn,[symbol:'cs492 system for artificial intelligence',syntax:[cs492,system,for,artificial,intelligence]]).
lexEntry(pn,[symbol:'cs492 systems for machine learning',syntax:[cs492,systems,for,machine,learning]]).
lexEntry(pn,[symbol:'cs492 tech for impact',syntax:[cs492,tech,for,impact]]).
lexEntry(pn,[symbol:'cs492 technology for impact',syntax:[cs492,technology,for,impact]]).
lexEntry(pn,[symbol:'cs492 trust and safety engineering and practice',syntax:[cs492,trust,and,safety,engineering,and,practice]]).
lexEntry(pn,[symbol:'cs492 unconventional computing',syntax:[cs492,unconventional,computing]]).
lexEntry(pn,[symbol:'cs492 virtualization',syntax:[cs492,virtualization]]).
lexEntry(pn,[symbol:'cs492 wearable interface',syntax:[cs492,wearable,interface]]).
lexEntry(pn,[symbol:'cs492 web security attack laboratory',syntax:[cs492,web,security,attack,laboratory]]).
lexEntry(pn,[symbol:'cs493 cs for all',syntax:[cs493,cs,for,all]]).
lexEntry(pn,[symbol:'cs500',syntax:[cs500]]).
lexEntry(pn,[symbol:'cs504',syntax:[cs504]]).
lexEntry(pn,[symbol:'cs510',syntax:[cs510]]).
lexEntry(pn,[symbol:'cs520',syntax:[cs520]]).
lexEntry(pn,[symbol:'cs524',syntax:[cs524]]).
lexEntry(pn,[symbol:'cs530',syntax:[cs530]]).
lexEntry(pn,[symbol:'cs540',syntax:[cs540]]).
lexEntry(pn,[symbol:'cs543',syntax:[cs543]]).
lexEntry(pn,[symbol:'cs546',syntax:[cs546]]).
lexEntry(pn,[symbol:'cs548',syntax:[cs548]]).
lexEntry(pn,[symbol:'cs550',syntax:[cs550]]).
lexEntry(pn,[symbol:'cs560',syntax:[cs560]]).
lexEntry(pn,[symbol:'cs564',syntax:[cs564]]).
lexEntry(pn,[symbol:'cs565',syntax:[cs565]]).
lexEntry(pn,[symbol:'cs570',syntax:[cs570]]).
lexEntry(pn,[symbol:'cs572',syntax:[cs572]]).
lexEntry(pn,[symbol:'cs575',syntax:[cs575]]).
lexEntry(pn,[symbol:'cs576',syntax:[cs576]]).
lexEntry(pn,[symbol:'cs577',syntax:[cs577]]).
lexEntry(pn,[symbol:'cs579',syntax:[cs579]]).
lexEntry(pn,[symbol:'cs580',syntax:[cs580]]).
lexEntry(pn,[symbol:'cs584',syntax:[cs584]]).
lexEntry(pn,[symbol:'cs588',syntax:[cs588]]).
lexEntry(pn,[symbol:'cs592 advanced humancomputer interaction',syntax:[cs592,advanced,humancomputer,interaction]]).
lexEntry(pn,[symbol:'cs592 practical logic',syntax:[cs592,practical,logic]]).
lexEntry(pn,[symbol:'cs592 robot learning and interaction',syntax:[cs592,robot,learning,and,interaction]]).
lexEntry(pn,[symbol:'cs592 sensor data science',syntax:[cs592,sensor,data,science]]).
lexEntry(pn,[symbol:'cs592 introduction to program analysis',syntax:[cs592,introduction,to,program,analysis]]).
lexEntry(pn,[symbol:'cs610',syntax:[cs610]]).
lexEntry(pn,[symbol:'cs612',syntax:[cs612]]).
lexEntry(pn,[symbol:'cs632',syntax:[cs632]]).
lexEntry(pn,[symbol:'cs662',syntax:[cs662]]).
lexEntry(pn,[symbol:'cs665',syntax:[cs665]]).
lexEntry(pn,[symbol:'cs671',syntax:[cs671]]).
lexEntry(pn,[symbol:'cs672',syntax:[cs672]]).
lexEntry(pn,[symbol:'cs686',syntax:[cs686]]).
lexEntry(pn,[symbol:'cs688',syntax:[cs688]]).
lexEntry(pn,[symbol:'cs700 computer science of numerics',syntax:[cs700,computer,science,of,numerics]]).
lexEntry(pn,[symbol:'cs730',syntax:[cs730]]).
lexEntry(pn,[symbol:'cs770 artificial intelligent deepfake detection',syntax:[cs770,artificial,intelligent,deepfake,detection]]).
lexEntry(pn,[symbol:'cs772 korean corpus and semantic web',syntax:[cs772,korean,corpus,and,semantic,web]]).
lexEntry(pn,[symbol:'cs774 ai and ethics',syntax:[cs774,ai,and,ethics]]).
lexEntry(pn,[symbol:'cs790',syntax:[cs790]]).
lexEntry(pn,[symbol:'cs891 climate tech and startup',syntax:[cs891,climate,tech,and,startup]]).
lexEntry(pn,[symbol:'cs893 deep learning design theory',syntax:[cs893,deep,learning,design,theory]]).
lexEntry(pn,[symbol:'cs893 startup design',syntax:[cs893,startup,design]]).

/*========================================================================
   Proper Names: Number of courses
========================================================================*/

lexEntry(pn,[symbol:1,syntax:[1,course]]).
lexEntry(pn,[symbol:2,syntax:[2,courses]]).
lexEntry(pn,[symbol:3,syntax:[3,courses]]).
lexEntry(pn,[symbol:4,syntax:[4,courses]]).
lexEntry(pn,[symbol:5,syntax:[5,courses]]).
lexEntry(pn,[symbol:6,syntax:[6,courses]]).
lexEntry(pn,[symbol:7,syntax:[7,courses]]).
lexEntry(pn,[symbol:8,syntax:[8,courses]]).
lexEntry(pn,[symbol:9,syntax:[9,courses]]).
lexEntry(pn,[symbol:10,syntax:[10,courses]]).
lexEntry(pn,[symbol:2,syntax:[2,course]]).
lexEntry(pn,[symbol:3,syntax:[3,course]]).
lexEntry(pn,[symbol:4,syntax:[4,course]]).
lexEntry(pn,[symbol:5,syntax:[5,course]]).
lexEntry(pn,[symbol:6,syntax:[6,course]]).
lexEntry(pn,[symbol:7,syntax:[7,course]]).
lexEntry(pn,[symbol:8,syntax:[8,course]]).
lexEntry(pn,[symbol:9,syntax:[9,course]]).
lexEntry(pn,[symbol:10,syntax:[10,course]]).

/*========================================================================
   Transitive Verbs
========================================================================*/

lexEntry(tv,[symbol:want,syntax:[wanna],inf:fin,num:sg]).
lexEntry(tv,[symbol:want,syntax:[want,to],inf:fin,num:sg]).

lexEntry(tv,[symbol:plan,syntax:[wanna,plan,for],inf:fin,num:sg]).
lexEntry(tv,[symbol:plan,syntax:[wanna,plan,for,the],inf:fin,num:sg]).
lexEntry(tv,[symbol:plan,syntax:[want,to,plan,for],inf:fin,num:sg]).
lexEntry(tv,[symbol:plan,syntax:[want,to,plan,for,the],inf:fin,num:sg]).

lexEntry(tv,[symbol:interest,syntax:[am,interested,in],inf:fin,num:sg]).
lexEntry(tv,[symbol:interest,syntax:[like],inf:fin,num:sg]).
lexEntry(tv,[symbol:interest,syntax:[interested,in],inf:fin,num:sg]).

lexEntry(tv,[symbol:took,syntax:[took],inf:fin,num:sg]).

lexEntry(tv,[symbol:mustTake,syntax:[must,take],inf:fin,num:sg]).

lexEntry(tv,[symbol:planTake,syntax:[plan,to,take],inf:fin,num:sg]).

lexEntry(tv,[symbol:taking,syntax:[am,taking],inf:fin,num:sg]).
lexEntry(tv,[symbol:taking,syntax:[taking],inf:fin,num:sg]).

/*========================================================================
   Copula
========================================================================*/

lexEntry(cop,[pol:pos,syntax:[am],inf:fin,num:sg]).

/*========================================================================
   Coordinations
========================================================================*/

lexEntry(coord,[syntax:[and],type:conj]).
