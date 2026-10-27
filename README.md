# Intuition Generation Project
Repository for storing supplementary materials to accompany "Sources of Philosophical Intuitions: Towards a Model of Intuition Generation" project.

**Table of contents:**
- [The Extended List of References](https://github.com/DominikDziedzic/IntuitionGenerationProject/blob/main/README.md#the-extended-list-of-references)
- [Pilot Studies](https://github.com/DominikDziedzic/IntuitionGenerationProject/blob/main/README.md#pilot-studies)
- [Data Collection Instruments](https://github.com/DominikDziedzic/IntuitionGenerationProject/blob/main/README.md#data-collection-instruments)
- [Statistical Analysis](https://github.com/DominikDziedzic/IntuitionGenerationProject/blob/main/README.md#statistical-analysis)

## The Extended List of References

To view the extended list of references created with the project in mind:
- in .pdf:
  - visit [**List of References.pdf**](https://github.com/DominikDziedzic/IntuitionGenerationProject/blob/main/List%20of%20References%20-%20Towards%20a%20Model%20of%20Intuition%20Generation.pdf),
  - or right-click and select Save as [**List of References.pdf**](https://github.com/DominikDziedzic/IntuitionGenerationProject/raw/main/List%20of%20References%20-%20Towards%20a%20Model%20of%20Intuition%20Generation.pdf),
- in .rtf:
  - right-click and select Save as [**List of References.rtf**](https://raw.githubusercontent.com/DominikDziedzic/IntuitionGenerationProject/main/List%20of%20References%20-%20Towards%20a%20Model%20of%20Intuition%20Generation.rtf),
- in .docx:
  - right-click and select Save as [**List of References.docx**](https://github.com/DominikDziedzic/IntuitionGenerationProject/raw/main/List%20of%20References%20-%20Towards%20a%20Model%20of%20Intuition%20Generation.docx),
  
---

## Pilot Studies

The project is currently in the pilot studies phase. To view the results of the pilot studies, visit:
- [**Pilot Study on Partitioning**](https://github.com/DominikDziedzic/PilotStudyPartitioning) that examined the interplay between pre-experimentally and experimentally acquired knowledge after limited training
- [**Pilot Study on Sampling**](https://github.com/DominikDziedzic/PilotStudySampling) that demonstrated the causal relationship between value, probability, and responses to David Kaplan's 'Carnap vs. Agnew' case concerning the reference of demonstratives

### Synopsis

To ensure generalizability, I plan to run similar studies on at least three widely discussed philosophical thought experiments. To this end, three of the following thought experiments will be converted to experimental materials and used consistently in all planned studies: a Gettier case, the ‘famous violinist’ case, the case of the ‘philosophical zombie’, the inverted spectrum scenario or ‘Mary the super-scientist’ thought experiment, the Molyneux’s problem or one of many cases concerning the relationship between perception and thinking (e.g., a case of switching two objects that look the same), Tyler Burge’s ‘arthritis’ thought experiment or David Kaplan’s ‘Carnap vs. Agnew’ case concerning demonstratives. These cases trigger intuitive judgments of different logical forms, e.g., the Gettier cases involve the application of the concept of knowledge (i.e. judgment of the form _x_ is _F_); the ‘famous violinist’ case triggers judgment of the soundness of analogy; the ‘philosophical zombie’ case triggers modal judgments. Showing that a default sampling mechanism influences intuitive claims of different logical forms would support the generalizability of the proposed theory across thought experiments and scenarios.

## Data Collection Instruments

The following research instruments have been developed for the project, each incorporating a variant of the well-known Gettier thought experiment.

### Partitioning: Prototype Experimental Study

To view the contents of the research instrument:
- in a web browser, click [**Partitioning: Prototype Experimental Study**](https://ls3.kognilab.pl/index.php/276361?lang=en),
- in .html:
  - visit [**Questionnaire 276361.html**](https://github.com/DominikDziedzic/IntuitionGenerationProject/blob/main/Questionnaire%20276361.html),
  - right-click and select Save as [**Questionnaire 276361.html**](https://github.com/DominikDziedzic/IntuitionGenerationProject/raw/main/Questionnaire%20276361.html).
 
This study examines the interplay of experimentally and pre-experimentally acquired cognitive resources. To this end, participants are randomly assigned to one of three experimental conditions. In the first condition—referred to as "primed"—the participant reads a scenario and then answers the following question: Does John really know that it's 12 or does he only believe it? In the second condition—referred to as "unprimed"—the questionnaire first displays a scenario, but instead of a question, the training phase follows immediately after. The third condition—referred to as "control"—is similar to the second one: the training phase immediately follows the scenario. All three conditions contain the training phase, in which the questionnaire displays subsequent reasons for a possible answer to the scenario, but training is primed in the first condition: the questionnaire tracks the initial answers and displays the reasons for the initial answers. Participants in the other two conditions are randomly assigned to one of the possible answers to the scenario and are trained on the subsequent reasons for that answer. On each training trial, participants categorize the provided reasons as the reasons for an answer and receive the following feedback:

> Please consider how the explanation provides a reason for the claim that John really knows [only believes] it's 12 o'clock. Make sure you understand how the explanation supports this claim before continuing.

In primed and unprimed conditions, the training phase is followed by the transfer phase, in which a random counter-reason is displayed to encourage the reorganization of acquired knowledge. Emphasis is on the "unprimed" condition, in which participants have the option of shifting their response strategy from one based on the experimentally acquired knowledge to one based on pre-experimental knowledge. All conditions conclude with the final answers to the scenario.


### Sampling: Prototype Experimental Study

To view the contents of the research instrument:
- in a web browser, click [**Sampling: Prototype Experimental Study**](https://ls3.kognilab.pl/index.php/937199?lang=en),
- in .html:
  - visit [**Questionnaire 937199.html**](https://github.com/DominikDziedzic/IntuitionGenerationProject/blob/main/Questionnaire%20937199.html),
  - right-click and select Save as [**Questionnaire 937199.html**](https://github.com/DominikDziedzic/IntuitionGenerationProject/raw/main/Questionnaire%20937199.html).
 
A series of studies is conducted to quantitatively estimate the impact of probability and value considerations on intuition generation, to demonstrate the causal relationship between value, probability, and participants' responses, and to test the possibility that some responses are purely probability-sensitive, while some others are purely value-sensitive. The questionnaire displays cues in which evaluative and probabilistic dimensions are manipulated. Participants use slider bars to report the perceived probability and value of an object. Both the evaluative and probabilistic dimensions are manipulated to elicit high/low probability/value judgments, e.g., John supports a candidate who has a positive/negative trait and many/few supporters:

> He strongly supports a candidate who promises to introduce policies that would benefit people in the most vulnerable social groups. John cares deeply about the well-being of these people. A recent poll found that 70% of voters support this candidate.

Mediation analysis will be used to test whether manipulating the described traits influences participants' responses via probability/value evaluations. Linear mixed-effects models with value and probability as fixed effects will be used for data analysis.

## Statistical Analysis

The project is accompanied by data analysis. All statistical analyses are conducted using the R environment for statistical computing. This section contains the R source code accompanying the respective studies.

To view the R source code accompanying the experimental study on Partitioning:
- for analysis of variance:
  - visit [**Analysis of Variance.R**](https://github.com/DominikDziedzic/IntuitionGenerationProject/blob/main/Analysis%20of%20Variance.R),
  - or right-click and select Save as [**Analysis of Variance.R**](https://github.com/DominikDziedzic/IntuitionGenerationProject/raw/main/Analysis%20of%20Variance.R),
- for analysis of linear regression in the frequentist and Bayesian frameworks:
  - visit [**Analysis of Linear Regression.R**](https://github.com/DominikDziedzic/IntuitionGenerationProject/blob/main/Analysis%20of%20Linear%20Regression.R),
  - or right-click and select Save as [**Analysis of Linear Regression.R**](https://github.com/DominikDziedzic/IntuitionGenerationProject/raw/main/Analysis%20of%20Linear%20Regression.R),

