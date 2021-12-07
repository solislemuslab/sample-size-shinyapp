    Aha, yes, hard question to answer!! Here is my strategy:

1. Tell them it’s a hard question, and that we typically cannot answer without preliminary data. Like a disclaimer, to get their hopes down right from the start.
2. Explain that the perfect sample depends on what they want to achieve (it does take some time to explain that), so I will ask questions to understand what they want to achieve.
3. Explain about power (in their words, using variable names like the ones in their experiment), then ask questions about what effect would be desirable to be detectable (that is, effect size for which they would like a low type 2 error rate). Ask lots of questions and make sure they understand.
4. Explain that the necessary sample size depends on the variability: in theory. Then ask about prior knowledge about the variability. Ideally, they have an estimated residual variance from preliminary data, but most often they don’t. It’s okay: there was a disclaimer at the start. In any case, they all tend to have a vague idea about the variability, if at least a range of values for the observations. That’s a start.
5. Tell them “thanks, I will use your information and get back to you”, or calculate the sample size right away with them on the spot (!!)

Now I assume that you know what they want to do: ANOVA, or linear mixed models, etc.
From there, pick 1 pairwise comparison: simplify what they will do to a single, simple pairwise comparison.
Forget about the global test that they will do, and calculate the power from this 1 comparison. Why?
1. calculating the desired sample size is simple to do for a single t-test, or single test for proportions.
2. it is so much simpler to explain the calculations. Any power / sample size calculation for ANOVA or other models is a lot more complicated… and for not much benefits, because they will end up doing pairwise comparisons in the end (most of the time).
3. it is so much simpler to interpret.
I know that #2 and #3 are about communication, but it’s important that they “own” their analysis and their choices. They will make the decision about the sample size, in the end, not you.

When I get back to them, I typically present a table with a range of options:
- to detect the effect you want to detect with 80% chance, and if the variance is this much: n=xxx (typically, this is going to be too big for them).
- then list other options, to get to a more manageable sample size, like: larger effect, or smaller variance.
Giving them a range of options gives them food for thought, like: motivation to get a large sample size, or motivation to get preliminary data before their full analysis, or motivation to revisit their experimental setup (like: decrease the number of time points (say) but increase the sample size at each time point).
So it’s good to meet a second time to help them think through the implications.