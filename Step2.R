load("Results/Result1.RData")
library(quanteda)
s1 <- data.frame(kwic(Programs, pattern = c(phrase("active learning"),
                                             phrase("new information"),
                                             phrase("future problem solving"),
                                             phrase("future decision-making"),
                                             phrase("implication"),
                                             phrase("implications"),
                                             phrase("current problems"),
                                             phrase("current solutions"),
                                             phrase("strategic marketing"))))
s1$Competence <- "Active Learning"
s1$SkillCode <- "S1"

s2 <- data.frame(kwic(Programs, pattern = c(phrase("active listening"),
                                            phrase("listening skills"),
                                            phrase("effective listening"),
                                            phrase("attentive listening"),
                                            phrase("listen actively"),
                                            phrase("understand others"),
                                            phrase("engaged listening"),
                                            phrase("client insights"),
                                            phrase("responsive listening"),
                                            phrase("listening to feedback"),
                                            phrase("customer feedback"),
                                            phrase("listening to clients"),
                                            phrase("mindfulness"),
                                            phrase("understanding"),
                                            phrase("information retention"),
                                            phrase("paraphrasing"))))
s2$Competence <- "Active Listening"
s2$SkillCode <- "S2"

s3 <- data.frame(kwic(Programs, pattern = c(phrase("critical thinking"),
                                            phrase("think critically"), 
                                            phrase("critical analysis"),
                                            phrase("reasoning skills"),
                                            phrase("analytical thinking"),
                                            phrase("evaluative thinking"),
                                            phrase("critical evaluation"),
                                            phrase("strategic analysis"),
                                            phrase("problem analysis"),
                                            phrase("critical problem solving"),
                                            phrase("analytical reasoning"),
                                            phrase("evaluating information"),
                                            phrase("argumentation"),
                                            phrase("logic"),
                                            phrase("reasoning"),
                                            phrase("complex problem-solving"))))
s3$Competence <- "Critical Thinking"
s3$SkillCode <- "S3"


s4 <- data.frame(kwic(Programs, pattern = c(phrase("reading comprehension"),
                                            phrase("written sentences"), 
                                            phrase("understanding documents"),
                                            phrase("work-related documents"),
                                            phrase("read"),
                                            phrase("reading communications"),
                                            phrase("understanding texts"))))
s4$Competence <- "Reading Comprehension"
s4$SkillCode <- "S4"

s5 <- data.frame(kwic(Programs, pattern = c(phrase("social perception"),
                                            phrase("empathy"), 
                                            phrase("understanding others"),
                                            phrase("social awareness"),
                                            phrase("social sensitivity"),
                                            phrase("others reactions"),
                                            phrase("social behavior"))))
s5$Competence <- "Social Perceptiveness"
s5$SkillCode <- "S5"

s6 <- data.frame(kwic(Programs, pattern = c(phrase("speaking"),
                                            phrase("talking"),
                                            phrase("telling stories"),
                                            phrase("communicating"),
                                            phrase("effectively"),
                                            phrase("talking with others"),
                                            phrase("conveying information"))))
s6$Competence <- "Speaking"
s6$SkillCode <- "S6"

s7 <- data.frame(kwic(Programs, pattern = c(phrase("decision making"),
                                            phrase("making decisions"),
                                            phrase("judgment skills"),
                                            phrase("decisive action"),
                                            phrase("evaluate options"),
                                            phrase("choose solutions"),
                                            phrase("decision strategies"),
                                            phrase("strategic decision making"),
                                            phrase("decisive judgment"),
                                            phrase("business decision making"),
                                            phrase("executive decision making"),
                                            phrase("critical judgment"),
                                            phrase("risk assessment"),
                                            phrase("intuition"),
                                            phrase("experience"),
                                            phrase("confidence"),
                                            phrase("accountability"),
                                            phrase("leadership"))))
s7$Competence <- "Decision Making and Judgment"
s7$SkillCode <- "S7"


s8 <- data.frame(kwic(Programs, pattern = c(phrase("monitoring"), 
                                           phrase("assessing performance"),
                                           phrase("self-assessment"),
                                           phrase("corrective action"),
                                           phrase("make improvement"),
                                           phrase("organization performance"),
                                           phrase("individual performance"),
                                           phrase("enhance"),
                                           phrase("enhancement"))))
s8$Competence <- "Monitoring"
s8$SkillCode <- "S8"

s9 <- data.frame(kwic(Programs, pattern = c(phrase("persuading"), 
                                         phrase("persuading others"),
                                         phrase("persuading skills"),
                                         phrase("influencing others"),
                                         phrase("convincing others"),
                                         phrase("selling ideas"),
                                         phrase("effective persuasion"),
                                         phrase("persuasive communication"),
                                         phrase("influence marketing"),
                                         phrase("marketing influence"),
                                         phrase("persuasive skills"),
                                         phrase("persuasive strategies"),
                                         phrase("social influence"),
                                         phrase("argumentation"),
                                         phrase("appealing to emotions"),
                                         phrase("motivation"),
                                         phrase("credibility"),
                                         phrase("trust"),
                                         phrase("convince"),
                                         phrase("persuasion"),
                                         phrase("change behavior"),
                                         phrase("change minds"),
                                         phrase("persuade"))))
s9$Competence <- "Persuasion"
s9$SkillCode <- "S9"


s10 <- data.frame(kwic(Programs, pattern = c(phrase("complex problem solving"), 
                                             phrase("complex problems"),
                                             phrase("implement solutions"),
                                             phrase("evaluate options"),
                                             phrase("complexity"),
                                             phrase("review information"),
                                             phrase("develop solutions"),
                                             phrase("identify problems"))))
s10$Competence <- "Complex Problem Solving"
s10$SkillCode <- "S10"

s11 <- data.frame(kwic(Programs, pattern = c(phrase("negotiation"),
                                            phrase("negotiating skills"),
                                            phrase("bargaining"),
                                            phrase("reconcile differences"),
                                            phrase("deal making"),
                                            phrase("conflict resolution"),
                                            phrase("mediate"),
                                            phrase("negotiation tactics"),
                                            phrase("win-win negotiation"),
                                            phrase("negotiation strategies"),
                                            phrase("negotiation skills"),
                                            phrase("business negotiation"),
                                            phrase("contract negotiation"),
                                            phrase("price negotiation"),
                                            phrase("deal negotiation"),
                                            phrase("conflict negotiation"),
                                            phrase("intercultural negotiation"),
                                            phrase("global negotiation"))))
s11$Competence <- "Negotiation"
s11$SkillCode <- "S11"

s12 <- data.frame(kwic(Programs, pattern = c(phrase("coordination"),
                                             phrase("coordinat*"),
                                             phrase("adjusting actions"),
                                             phrase("coordinate behavior"),
                                             phrase("cooperate with others"),
                                             phrase("cooperat*"),
                                             phrase("collaborat*"),
                                             phrase("coordinate tasks"),
                                             phrase("team coordination"),
                                             phrase("organizational skills"),
                                             phrase("work together"),
                                             phrase("task management"),
                                             phrase("coordinating activities"),
                                             phrase("inter-department coordination"),
                                             phrase("coordinated efforts"),
                                             phrase("team coordination strategies"),
                                             phrase("collaborative coordination"),
                                             phrase("project management"),
                                             phrase("effective communication"),
                                             phrase("delegation"),
                                             phrase("conflict resolution"),
                                             phrase("leadership"),
                                             phrase("tracking"),
                                             phrase("evaluation"),
                                             phrase("collaborate with others"))))
s12$Competence <- "Coordination"
s12$SkillCode <- "S12"


s13 <- data.frame(kwic(Programs, pattern = c(phrase("systems evaluation"),
                                             phrase("identifying measures"),
                                             phrase("identifying indicators"),
                                             phrase("system performance"),
                                             phrase("needs"),
                                             phrase("improve performance"),
                                             phrase("correct performance"),
                                             phrase("goals of the system"),
                                             phrase("systems thinking"),
                                             phrase("systemic"))))
s13$Competence <- "Systems Evaluation"
s13$SkillCode <- "S13"


s14 <- data.frame(kwic(Programs, pattern = c(phrase("time management"), 
                                             phrase("timing"),
                                             phrase("managing one's own time"),
                                             phrase("managing time of others"),
                                             phrase("punctual"),
                                             phrase("on time"),
                                             phrase("time management skills"),
                                             phrase("organize time"),
                                             phrase("efficient scheduling"),
                                             phrase("prioritize tasks"),
                                             phrase("time optimization"),
                                             phrase("schedule management"),
                                             phrase("task prioritization"),
                                             phrase("effective time management"),
                                             phrase("time management strategies"),
                                             phrase("organization"),
                                             phrase("planning"),
                                             phrase("delegation"),
                                             phrase("multitasking"),
                                             phrase("stress management"),
                                             phrase("productivity"),
                                             phrase("efficiency"),
                                             phrase("self-discipline"),
                                             phrase("timely"),
                                             phrase("punctuality"))))
s14$Competence <- "Time Management"
s14$SkillCode <- "S14"

s15 <- data.frame(kwic(Programs, pattern = c(phrase("supervision"),
                                             phrase("supervise"),
                                             phrase("supervising"),
                                             phrase("management"),
                                             phrase("manage personnel"),
                                             phrase("manage staff"),
                                             phrase("team leadership"),
                                             phrase("lead teams"),
                                             phrase("employee supervision"),
                                             phrase("staff management"),
                                             phrase("effective supervision"),
                                             phrase("motivation"),
                                             phrase("coaching"),
                                             phrase("team development"),
                                             phrase("performance management"),
                                             phrase("discipline"),
                                             phrase("ethics"),
                                             phrase("change management"))))
s15$Competence <- "Supervision and Personnel Management"
s15$SkillCode <- "S15"



s16 <- data.frame(kwic(Programs, pattern = c(phrase("operation analysis"),
                                             phrase("analyzing needs"),
                                             phrase("analysing needs"),
                                             phrase("product requirement"),
                                             phrase("create designs"),
                                             phrase("designing products"),
                                             phrase("product design"),
                                             phrase("marketing products"),
                                             phrase("design of products"))))


s17 <- data.frame(kwic(Programs, pattern = c(phrase("systems analysis"),
                                             phrase("determining how"),
                                             phrase("systems should work"),
                                             phrase("outcome changes"),
                                             phrase("system conditions"),
                                             phrase("system functioning"),
                                             phrase("working system"),
                                             phrase("system operations"))))

s18 <- data.frame(kwic(Programs, pattern = c(phrase("written communication"),
                                            phrase("writing skills"),
                                            phrase("professional writing"),
                                            phrase("report writing"),
                                            phrase("business writing"),
                                            phrase("create marketing content"),
                                            phrase("marketing reports"),
                                            phrase("content creation"),
                                            phrase("marketing copywriting"),
                                            phrase("marketing messages"),
                                            phrase("written marketing materials"),
                                            phrase("writing for marketing"),
                                            phrase("persuasive writing"),
                                            phrase("creative writing"),
                                            phrase("proposal writing"),
                                            phrase("copywriting"),
                                            phrase("web content writing"),
                                            phrase("blogging"),
                                            phrase("social media writing"))))
s18$Competence <- "Written Communication"
s18$SkillCode <- "S18"


s19 <- data.frame(kwic(Programs, pattern = c(phrase("learning strategies"),
                                             phrase("learning strategy"),
                                             phrase("teaching strategy"),
                                             phrase("teaching strategies"),
                                             phrase("training"),
                                             phrase("instructional methods"),
                                             phrase("appropriate learning"),
                                             phrase("teaching new things"),
                                             phrase("learning new things"),
                                             phrase("selecting and using"),
                                             phrase("learning procedures"))))
s19$Competence <- "Learning Strategies"
s19$SkillCode <- "S19"


















s1 <- data.frame(kwic(Programs, pattern = c(phrase("oral communication"), 
                                            phrase("verbal communication"),
                                            phrase("public speaking"),
                                            phrase("presentation skills"),
                                            phrase("speak confidently"),
                                            phrase("effective speaking"),
                                            phrase("presentation delivery"),
                                            phrase("communication proficiency"),
                                            phrase("communicate effectively"),
                                            phrase("verbal presentations"),
                                            phrase("oral presentations"),
                                            phrase("elocution"),
                                            phrase("storytelling"),
                                            phrase("rapport building"),
                                            phrase("intercultural communication"),
                                            phrase("global communication"))))
s1$Competence <- "Speaking"
s1$SkillCode <- "S1"








s6 <- data.frame(kwic(Programs, pattern = c(phrase("service orientation"),
                                            phrase("customer service"),
                                            phrase("client service"),
                                            phrase("service skills"),
                                            phrase("customer satisfaction"),
                                            phrase("service excellence"),
                                            phrase("client relations"),
                                            phrase("customer care"),
                                            phrase("service delivery"),
                                            phrase("customer-centric approach"),
                                            phrase("service-oriented"),
                                            phrase("customer-focused"),
                                            phrase("customer loyalty"),
                                            phrase("positive attitude"),
                                            phrase("proactivity"),
                                            phrase("teamwork"))))
s6$Competence <- "Service Orientation"
s6$SkillCode <- "S6"



s8 <- data.frame(kwic(Programs, pattern = c(phrase("solving complex problems"),
                                            phrase("problem solving"),
                                            phrase("resolve issues"),
                                            phrase("troubleshooting"),
                                            phrase("conflict resolution"),
                                            phrase("creative solutions"),
                                            phrase("address challenges"),
                                            phrase("problem resolution"),
                                            phrase("overcome obstacles"),
                                            phrase("complex problem solving"),
                                            phrase("strategic problem solving"),
                                            phrase("problem analysis"),
                                            phrase("root cause identification"),
                                            phrase("solution generation"),
                                            phrase("solution evaluation"),
                                            phrase("solution implementation"),
                                            phrase("monitoring and evaluation"),
                                            phrase("adaptability"),
                                            phrase("flexibility"),
                                            phrase("persistence"))))
s8$Competence <- "Solving Complex Problems"
s8$SkillCode <- "S8"






s13 <- data.frame(kwic(Programs, pattern = c(phrase("creativity"),
                                             phrase("creative thinking"),
                                             phrase("innovative ideas"),
                                             phrase("generate ideas"),
                                             phrase("brainstorming"),
                                             phrase("original concepts"),
                                             phrase("creative solutions"),
                                             phrase("creative strategies"),
                                             phrase("innovation in marketing"),
                                             phrase("creative marketing"),
                                             phrase("creative skills"),
                                             phrase("ideation"),
                                             phrase("creative problem-solving"),
                                             phrase("lateral thinking"),
                                             phrase("design thinking"),
                                             phrase("risk-taking"),
                                             phrase("experimentation"),
                                             phrase("adaptability"))))
s13$Competence <- "Creativity"
s13$SkillCode <- "S13"

s14 <- data.frame(kwic(Programs, pattern = c(phrase("innovation"),
                                             phrase("innovative thinking"),
                                             phrase("disruptive thinking"),
                                             phrase("creativity"),
                                             phrase("innovation management"),
                                             phrase("innovative solutions"),
                                             phrase("creative problem solving"),
                                             phrase("fostering innovation"),
                                             phrase("leading innovation"),
                                             phrase("change management"),
                                             phrase("culture of innovation"),
                                             phrase("idea implementation"))))
s14$Competence <- "Innovation"
s14$SkillCode <- "S14"

s15 <- data.frame(kwic(Programs, pattern = c(phrase("marketing strategies design"),
                                             phrase("strategic marketing"),
                                             phrase("marketing planning"),
                                             phrase("market segmentation"),
                                             phrase("brand positioning"),
                                             phrase("product development"),
                                             phrase("pricing strategies"),
                                             phrase("promotion strategies"),
                                             phrase("distribution strategies"),
                                             phrase("marketing research"),
                                             phrase("marketing analytics"),
                                             phrase("marketing campaigns"),
                                             phrase("digital marketing"),
                                             phrase("social media marketing"),
                                             phrase("content marketing"),
                                             phrase("search engine marketing"),
                                             phrase("email marketing"),
                                             phrase("marketing automation"),
                                             phrase("marketing ROI"))))
s15$Competence <- "Marketing Strategies Design"
s15$SkillCode <- "S15"



df_list <- mget(paste0("s", 1:15))
SS <- do.call(rbind, df_list)
patrones <- data.frame(table(SS$pattern))
Patrones <- data.frame(table(SS$Competence))
SS <- merge(SS, TextosData, by.x = "docname", by.y = "Text")
#rm(list=setdiff(ls(), "SS"))
save.image("Results/Result2.RData")



rm(list=setdiff(ls(), "SS"))
save.image("Results/Result2.RData")

