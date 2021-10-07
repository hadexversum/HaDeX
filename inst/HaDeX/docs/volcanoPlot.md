## Volcano Plot

The volcano plot shows the deuterium uptake difference for two biological states for peptide and its p-value for double testing on statistical significance (Weis et al.). On the x-axis, there is a deuterium uptake difference with its uncertainty (combined and propagated). On the y-axis, there is a P-value calculated for each peptide in a specifc time point of a measurement as a un-paired t-test on given significance level (on mass measurement from the replicates to indicate if the measured mean is significantly different between two states, as the deuterium uptake difference between states can be rewritten as

$$Delta D=D_A-D_B=m_{t,A}-m_0-(m_{t,B}-m_0)=m_{t,A}-m_{t,B}$$


for states A and B. The values of deuterium uptake difference from all time points are shown on the plot.

The dotted red lines indicate confidence limits for the values. The horizontal line indicates the confidence limit based on chosen confidence level to give a threshold on a P-value. The vertical lines indicate the confidence limit from Houde test for all time points and indicate a threshold on deuterium uptake difference. The statistically significant points are in the top left and right corners of the plot.

For more information see the [documentation](https://hadexversum.github.io/HaDeX/articles/visualization.html#volcano-plot-1).

