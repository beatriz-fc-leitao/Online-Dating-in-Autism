* Encoding: UTF-8.
DATASET ACTIVATE DataSet1.

SORT CASES BY random_profile(A).

*Recoding wrong numbers.
RECODE Q213 Q214 Q216 Q217 (1=6) (2=7) (3=8) (4=9) (5=10).
EXECUTE.

RECODE Q176 Q177 Q179 Q180 (1=6) (2=7) (3=8) (4=9) (5=10).
EXECUTE.

RECODE Q250 Q251 Q253 Q254 (1=6) (2=7) (3=8) (4=9) (5=10).
EXECUTE.

RECODE Q41 Q42 Q44 Q45 (1=6) (2=7) (3=8) (4=9) (5=10).
EXECUTE.

RECODE date_3 (18=6) (19=7) (20=8) (21=9) (22=10).
EXECUTE.

*Combining variables into one column.
COMPUTE physical_1=sum.1(Q196, Q159, Q233, Q24).
EXECUTE.

COMPUTE physical_2=sum.1(Q197, Q160, Q234, Q25).
EXECUTE.

COMPUTE physical_3=sum.1(Q198, Q161, Q235, Q26).
EXECUTE.

COMPUTE social_1=sum.1(Q183, Q146, Q220, Q11).
EXECUTE.

COMPUTE social_2=sum.1(Q184, Q147, Q221, Q12).
EXECUTE.

COMPUTE social_3=sum.1(Q185, Q148, Q222, Q13).
EXECUTE.

COMPUTE social_4=sum.1(Q186, Q149, Q223, Q14).
EXECUTE.

COMPUTE social_5=sum.1(Q187, Q150, Q224, Q15).
EXECUTE.

COMPUTE social_6=sum.1(Q188, Q151, Q225, Q16).
EXECUTE.

COMPUTE social_7=sum.1(Q189, Q152, Q226, Q17).
EXECUTE.

COMPUTE social_8=sum.1(Q190, Q153, Q227, Q18).
EXECUTE.

COMPUTE social_9=sum.1(Q191, Q154, Q228, Q19).
EXECUTE.

COMPUTE social_10=sum.1(Q192, Q155, Q229, Q20).
EXECUTE.

COMPUTE social_11=sum.1(Q193, Q156, Q230, Q21).
EXECUTE.

COMPUTE social_12=sum.1(Q194, Q157, Q231, Q22).
EXECUTE.

COMPUTE task_1=sum.1(Q200, Q163, Q237, Q28).
EXECUTE.

COMPUTE task_2=sum.1(Q201, Q164, Q238, Q29).
EXECUTE.

COMPUTE task_3=sum.1(Q202, Q165, Q239, Q30).
EXECUTE.

COMPUTE task_4=sum.1(Q203, Q166, Q240, Q31).
EXECUTE.

COMPUTE task_5=sum.1(Q204, Q167, Q241, Q32).
EXECUTE.

COMPUTE honesty_1=sum.1(Q206, Q169, Q243, Q34).
EXECUTE.

COMPUTE honesty_2=sum.1(Q207, Q170, Q244, Q35).
EXECUTE.

COMPUTE honesty_3=sum.1(Q208, Q171, Q245, Q36).
EXECUTE.

COMPUTE honesty_4=sum.1(Q209, Q172, Q246, Q37).
EXECUTE.

COMPUTE honesty_5=sum.1(Q210, Q173, Q38, Q247).
EXECUTE.

COMPUTE honesty_6=sum.1(Q211, Q174, Q248, Q39).
EXECUTE.

COMPUTE date_1=sum.1(Q213, Q176, Q250, Q41).
EXECUTE.

COMPUTE date_2=sum.1(Q214, Q177, Q251, Q42).
EXECUTE.

COMPUTE date_3=sum.1(Q215, Q178, Q252, Q43).
EXECUTE.

COMPUTE date_4=sum.1(Q216, Q179, Q253, Q44).
EXECUTE.

COMPUTE date_5=sum.1(Q217, Q180, Q254, Q45).
EXECUTE.

*Recoding reversed ones: physical - 2, 3; social - 3, 4, 5, 8, 11, 12; task - 1, 4, 5; honesty - 2, 5.
RECODE physical_2 physical_3 social_3 social_4 social_5 social_8 social_11 social_12 task_1 task_4 
    task_5 honesty_2 honesty_5 (6=10) (7=9) (8=8) (9=7) (10=6).
EXECUTE.

*Physical mean.
COMPUTE mean_physical=MEAN(physical_1, physical_2, physical_3).
EXECUTE.

*Social mean.
COMPUTE mean_social=MEAN(social_1, social_2, social_3, social_4, social_5, social_6, social_7, 
    social_8, social_9, social_10, social_11, social_12).
EXECUTE.

*Task mean.
COMPUTE mean_task=MEAN(task_1, task_2, task_3, task_4, task_5).
EXECUTE.

*Honesty mean.
COMPUTE mean_honesty=MEAN(honesty_1, honesty_2, honesty_3, honesty_4, honesty_5, honesty_6).
EXECUTE.

*Date mean.
COMPUTE mean_date=MEAN(date_1, date_2, date_3, date_4, date_5).
EXECUTE.

*Total stigma: Q484_1 - Q484_6.
COMPUTE total_stigma=SUM(Q484_1, Q484_2, Q484_3, Q484_4, Q484_5, Q484_6).
EXECUTE.

RECODE Q213 Q214 Q216 Q217 (1=6) (2=7) (3=8) (4=9) (5=10).
EXECUTE.

*Recoding stigma - 0 = no stigma, 24 = lots of stigma.
DATASET ACTIVATE DataSet1.
RECODE total_stigma (0=24) (1=23) (2=22) (3=21) (4=20) (5=19) (6=18) (7=17) (8=16) (9=15) (10=14) 
    (11=13) (12=12) (13=11) (14=10) (15=9) (16=8) (17=7) (18=6) (19=5) (20=4) (21=3) (22=2) (23=1) 
    (24=0) INTO Updated_total_stigma.
EXECUTE.






*MANCOVA not significant.
GLM mean_physical mean_social mean_task mean_honesty mean_date BY Stigmatiser Label Wording WITH 
    HighestExperience
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=ETASQ
  /CRITERIA=ALPHA(.05)
  /DESIGN=HighestExperience Stigmatiser Label Wording Stigmatiser*Label Stigmatiser*Wording 
    Label*Wording Stigmatiser*Label*Wording.


*MANOVA.
GLM mean_physical mean_social mean_task mean_honesty mean_date BY Stigmatiser Label Wording
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT=DESCRIPTIVE ETASQ OPOWER HOMOGENEITY
  /CRITERIA=ALPHA(.05)
  /DESIGN= Stigmatiser Label Wording Stigmatiser*Label Stigmatiser*Wording Label*Wording 
    Stigmatiser*Label*Wording.
