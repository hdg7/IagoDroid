# IagoDroid

IagoDroid is a tool focused on cheating a classifier. The tool modifies static features of a malicious app to change its family classification. To show an example 
of how IagoDroid performs in real environment, we have include an implementation of [RevealDroid](https://bitbucket.org/joshuaga/revealdroid/overview)

IagoDroid was presented in:

Calleja, A., Martín, A., Menéndez, H. D., Tapiador, J., & Clark, D. (2018). Picking on the family: Disrupting android malware triage by forcing misclassification. Expert Systems with Applications, 95, 113-126.

## Working with IagoDroid

For activating the tool you need: a model (in this case it is adapted for J48 kind of models) and a list of individuals.

```
./IagoDroid model.rds individuals.txt individual increment maxIterations outputFolder
```  

The model.rds file is the model to cheat. The individuals.txt list is a group of individuals that will be reshape to cheat the classifier. From the list, you need to specify
the index for the individual you want to reshape. The output of the tool is a list of potential changes that you can include to your app in order to alterate its classification.

As parameters, you have the increment: this value is usually between 0.55 and 1. Having a low increment will suppose to find a low number of changes, but will require more 
effort to the GA to find them. The maxIterations is the maximum number of generations for the GA.

The outputFolder is the results folder where you will find the summary of the evolutionary process.

## RevealDroid

We have include an implementation of [RevealDroid](https://bitbucket.org/joshuaga/revealdroid/overview). You can run it with the paper data which is available here:

https://data.mendeley.com/datasets/4sksrpm5vj/

The software generates a model. This model is the input for IagoDroid.

./revealDroid.r total_arff_drebin_reduced.arff

