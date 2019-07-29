import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn import neighbors
from sklearn.metrics import auc, roc_curve, accuracy_score
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import train_test_split, KFold, cross_val_score
from sklearn.ensemble import RandomForestClassifier
from sklearn.preprocessing import StandardScaler
# Import and suppress warnings
import warnings
warnings.filterwarnings('ignore')

# reading in the data
df = pd.read_csv("winequality-white.csv", sep = ";")
print(df.isnull().head()) # check for missing values

# correlation matrix
f, ax = plt.subplots(figsize=(10, 8))
corr = df.corr()
sns.heatmap(corr, mask=np.zeros_like(corr, dtype=np.bool),
            cmap=sns.diverging_palette(220, 10, as_cmap=True),
            square=True, ax=ax)
plt.show()

# dropping residual.sugar, density, and total.sulfur.dioxide
df.drop(["residual sugar", "density", "total sulfur dioxide"], axis = 1,
        inplace = True)

# new correlation matrix
f, ax = plt.subplots(figsize = (10, 8))
corr = df.corr()
sns.heatmap(corr, mask = np.zeros_like(corr, dtype=np.bool),
            cmap = sns.diverging_palette(220, 10, as_cmap=True),
            square = True, ax = ax)
plt.show()

# assigning the independent  variables to X
X = df.loc[:, 'fixed acidity':'alcohol']
feature_names = X.columns.values 
# converting the dependent variable from numeric to categorical
def score_to_label(x):
    if x > 5:
        return "good"
    else:
        return "bad"

# replacing the numeric 'quality' with categorical 'label'
df.quality = df.quality.apply(score_to_label)
df.quality, class_names = pd.factorize(df.quality)
y = df.quality

# split the data intro training and test sets, test size is 30% of the data
# train_test_split is loaded from sklearn.model_selection
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.3, random_state = 100)

# scaling the numeric attributes
# from sklearn.preprocessingimport StandardScaler
scaler = StandardScaler().fit(X_train)
# scaled X
X_train = scaler.transform(X_train) 
X_test = scaler.transform(X_test)

### Model Fitting ####
# decision tree classifier object
tree_clf = DecisionTreeClassifier(criterion = "gini", random_state = 100)
tree_clf.fit(X_train, y_train) # training the model
tree_y_pred = tree_clf.predict(X_test) # predicting

# Getting feature importances from model
tree_importance = pd.Series(tree_clf.feature_importances_, index = df.columns[0:8])
tree_importance.plot(kind = 'barh')
plt.show()

'''
from sklearn.tree import DecisionTreeClassifier
from IPython.display import Image  
from sklearn import tree
import pydotplus

# Create DOT data
dot_data = tree.export_graphviz(tree_clf, out_file=None, 
                                feature_names = X.columns.values,  
                                class_names = y.columns.values)

# Draw graph
graph = pydotplus.graph_from_dot_data(dot_data)  

# Show graph
Image(graph.create_png())
'''

def calculate_roc(clf, X_test, y_test):
    probs = clf.predict_proba(X_test)
    preds = probs[:,1]
    fpr, tpr, threshold = roc_curve(y_test, preds)
    roc_auc = auc(fpr, tpr)
    return roc_auc

# sklearn.metrics is loaded

def plot_roc_curve(clf, X_test, y_test, name):
    # calculating fpr, tpr and AUC
    probs = clf.predict_proba(X_test)
    preds = probs[:,1]
    fpr, tpr, threshold = roc_curve(y_test, preds)
    roc_auc = auc(fpr, tpr)
    
    # plotting
    title = 'ROC Curve, ' + name
    plt.title(title)
    plt.plot(fpr, tpr, 'b', label = 'AUC = %0.2f' % roc_auc)
    plt.legend(loc = 'lower right')
    plt.plot([0, 1], [0, 1],'r--')
    plt.xlim([0, 1])
    plt.ylim([0, 1])
    plt.ylabel('True Positive Rate')
    plt.xlabel('False Positive Rate')
    plt.show()

# metrics
tree_acc = accuracy_score(y_test, tree_y_pred)
tree_misclass = np.mean(tree_y_pred != y_test) # misclassification error
tree_roc = calculate_roc(tree_clf, X_test, y_test)
plot_roc_curve(tree_clf, X_test, y_test, "Decision Tree")

# dataFrame comparing all of the algorithms
records = []
records.append({'Algorithm': 'decision tree',
                'Accuracy Rate': tree_acc,
                'AUC': tree_roc,
                'MisclassRate': tree_misclass})

# k-Fold cross validation
# k = 10
depth = []
kf = KFold(n_splits = 10, shuffle = True, random_state = 100)
for i in range(3,20):
    test_clf = DecisionTreeClassifier(max_depth=i)
    test_clf.fit(X_train, y_train) # training the model
    test_y_pred = test_clf.predict(X_test) # making predictions
    misclass = np.mean(test_y_pred != y_test) # misclassification error
    scores = cross_val_score(estimator = test_clf, X=X, y=y, cv=10, n_jobs=-1)
    depth.append((i, misclass, scores.mean()))

# Comparing number of leaves to misclassification error
depth = pd.DataFrame(depth)
depth.columns = ['leaves','misclass_error', 'CV_score']

# plotting
def scatter_plot(df, x, y, title):
    # matplotlib is loaded as plt
    plt.scatter(df[x], df[y])
    plt.title(title)
    plt.xlabel(x)
    plt.ylabel(y)
    plt.show()

scatter_plot(depth, 'leaves', 'misclass_error', 'Number of leaves vs Misclassification Rate')
scatter_plot(depth, 'leaves', 'CV_score', 'Number of leaves vs Cross-Validation Score')

# the optimal number of leaves is 6
pruned_clf = DecisionTreeClassifier(max_depth = 6)
pruned_clf.fit(X_train, y_train)
pruned_y_pred = pruned_clf.predict(X_test)

pruned_acc = accuracy_score(y_test, pruned_y_pred)
pruned_misclass = np.mean(pruned_y_pred != y_test) # misclassification error
pruned_roc = calculate_roc(pruned_clf, X_test, y_test)
plot_roc_curve(pruned_clf, X_test, y_test, "Pruned Decision Tree")

# adding pruned
records.append({'Algorithm': 'CV pruned decision tree',
                'Accuracy Rate': pruned_acc,
                'AUC': pruned_roc,
                'MisclassRate': pruned_misclass})

# k-nearest neighbors
knn_clf = neighbors.KNeighborsClassifier(10, weights = "distance")
knn_clf.fit(X_train, y_train)
knn_y_pred = knn_clf.predict(X_test)

# metrics and ROC curve
knn_acc = accuracy_score(y_test, knn_y_pred)
knn_misclass = np.mean(knn_y_pred != y_test) # misclassification error
knn_roc = calculate_roc(knn_clf, X_test, y_test)
plot_roc_curve(knn_clf, X_test, y_test, "k-nearest neighbors")

# adding knn
records.append({'Algorithm': 'k-nearest neighbors, k = 10',
                'Accuracy Rate': knn_acc,
                'AUC': knn_roc,
                'MisclassRate': knn_misclass})

# initialize dataFrame to store accuracy rates and AUC for different values of k
knn_df = []
for k in range(1,21):
    # fitting and using the knn classifier
    # creating the classifier
    test_clf_knn = neighbors.KNeighborsClassifier(n_neighbors = k, weights = "distance")
    test_clf_knn.fit(X_train, y_train) # fitting the classifier
    test_knn_pred = test_clf_knn.predict(X_test) # predicting with the test set

    # Calculate AUC
    test_roc_auc = calculate_roc(test_clf_knn, X_test, y_test)

    knn_df.append({'k': k,
               'AccuracyRate': accuracy_score(y_test, test_knn_pred),
               'AUC': test_roc_auc,
               'MisclassRate': np.mean(test_knn_pred != y_test)})

knn_df = pd.DataFrame(knn_df)

scatter_plot(knn_df, 'k', 'AccuracyRate', 'Number of neighbors (k) vs Accuracy Rate')
scatter_plot(knn_df, 'k', 'AUC', 'Number of neighbors (k) vs AUC')
scatter_plot(knn_df, 'k', 'MisclassRate', 'Number of neighbors (k) vs Misclassification Rate')

# optimal seems to be k = 10, our original choice

# randomForest
rf_clf = RandomForestClassifier(bootstrap = True, criterion = 'gini', n_jobs = -1)
rf_clf.fit(X_train, y_train)
rf_y_pred = rf_clf.predict(X_test)

# metrics and plotting ROC curve
rf_acc = accuracy_score(y_test, rf_y_pred)
rf_misclass = np.mean(rf_y_pred != y_test) # misclassification error
rf_roc = calculate_roc(rf_clf, X_test, y_test)
plot_roc_curve(rf_clf, X_test, y_test, "Random Forest")

# feature importance
feat_importances = pd.Series(rf_clf.feature_importances_, index = df.columns[0:8])
feat_importances.nlargest(3).plot(kind='barh')

# adding random forest
records.append({'Algorithm': 'random forest',
                'Accuracy Rate': rf_acc,
                'AUC': rf_roc,
                'MisclassRate': rf_misclass})

# converting into a dataFrame
records = pd.DataFrame(records)
records = records[['Algorithm', 'Accuracy Rate', 'AUC', 'MisclassRate']]
print(records)
