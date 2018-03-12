library(data.table)
traindata = fread("train.csv")
testdata = fread("test.csv")

traindata$Survived <- as.factor(traindata$Survived)
traindata$Pclass <- as.factor(traindata$Pclass)
traindata$Sex <- as.factor(traindata$Sex)
traindata$Embarked <- as.factor(traindata$Embarked)
testdata$Pclass <- as.factor(testdata$Pclass)
testdata$Sex <- as.factor(testdata$Sex)
testdata$Embarked <- as.factor(testdata$Embarked)
combine <- bind_rows(traindata, testdata)

ggplot(traindata, aes(Pclass, fill = Survived)) + geom_bar(stat = "count", position = "dodge")
ggplot(traindata, aes(Sex, fill = Survived)) + geom_bar(stat = "count", position = "dodge")
ggplot(traindata, aes(Age, fill = Survived, color = Survived)) + geom_freqpoly(binwidth = 2)
ggplot(traindata, aes(Fare, fill = Survived, color = Survived)) + geom_freqpoly(binwidth = 1) + scale_x_log10()
ggplot(traindata, aes(Embarked, fill = Survived)) + geom_bar(stat = "count", position = "dodge")

traindata %>% 
        group_by(Survived) %>%
        summarise(mean_age = mean(Age, na.rm = TRUE))

traindata %>% 
        group_by(Survived) %>%
        summarise(most_age = median(Age, na.rm = TRUE))

traindata %>% 
        group_by(Sex) %>%
        summarise(mean_age = mean(Age, na.rm = TRUE))

traindata %>%
        count(Survived) %>% 
        group_by(Survived) %>%
        mutate(freq = n/nrow(traindata))

traindata %>%
        select(-PassengerId, -Name, -Cabin, -Ticket) %>%
        mutate(Sex = fct_recode(Sex,
                "0" = "male",
                "1" = "female")) %>%
        mutate(Sex = as.integer(Sex),
                Pclass = as.integer(Pclass),
                Survived = as.integer(Survived),
                Embarked = as.integer(Embarked)) %>%
        cor(use="complete.obs") %>%
        corrplot(type="lower", method="number")

traindata %>%
        ggplot(aes(Fare, fill=Pclass)) +
        geom_density(alpha = 0.5) +
        scale_x_log10() +
        facet_wrap(~ Survived, ncol = 1)

ggplot(traindata, aes(Pclass, Fare, colour = Survived)) +
        geom_boxplot() +
        scale_y_log10()

traindata %>%
        ggplot(mapping = aes(Fare, Age, color = Survived)) +
        geom_point() +
        scale_x_log10() +
        facet_grid(Embarked ~ Pclass)

traindata %>% 
        group_by(Pclass) %>%
        summarise(mean_age = mean(Age, na.rm = TRUE))

traindata %>% 
        group_by(Pclass) %>%
        summarise(median_age = median(Age, na.rm = TRUE))

traindata %>%
        ggplot(aes(Parch, SibSp, color = Survived)) +
        geom_count()

traindata %>%
        mutate(SibSp = factor(SibSp)) %>%
        ggplot(aes(Age, color = SibSp)) +
        geom_density(size = 2)

combine <- mutate(combine,
        fclass = factor(log10(Fare+1) %/% 1),
        title_orig = factor(str_extract(Name, "[A-Z][a-z]*\\.")),
        young = factor( if_else(Age<=30, 1, 0, missing = 0) | (title_orig %in% c('Master.','Miss.')) ),
        child = Age<10,
        family = SibSp + Parch,
        alone = (SibSp == 0) & (Parch == 0),
        large_family = (SibSp > 2) | (Parch > 3))

traindata <- combine %>% filter(!is.na(Survived))
testdata <- combine %>% filter(is.na(Survived))

