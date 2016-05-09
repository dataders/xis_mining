#load required libraries, data, and created functions
library(dplyr)
library(tm)
library(tidyr)
library(SnowballC)
library(RWeka)

#cleans and stems
CorpusClean <- function(corpus) {
        corpus.copy <- corpus
        corpus %>%
                tm_map(removePunctuation) %>%
                tm_map(stripWhitespace) %>%
                tm_map(content_transformer(tolower), lazy=FALSE) %>%
                # tm_map(removeWords, stopwords("en")) %>%
                tm_map(stemDocument, language = "en")
}

#completes stems
CorpusClean_mod <- function(corpus) {
        corpus.copy <- corpus
        corpus %>%
                tm_map(removePunctuation) %>%
                tm_map(stripWhitespace) %>%
                tm_map(content_transformer(tolower), lazy=FALSE) %>%
                # tm_map(removeWords, stopwords("en")) %>%
                tm_map(stemDocument)  %>% 
                tm_map(content_transformer(function(x, corpus.copy) paste(
                        stemCompletion(strsplit(stemDocument(x), ' ')[[1]], corpus.copy),
                        collapse = ' ')), corpus.copy)
}


#Get Corpus, Clean & Stem from Atls Skill Cluster DF
GetCorpusFromClusters <- function(clusters) {
        cluster.names <- c("Communication","Collaboration","Organization",
                           "Affective","Reflection","Information Literacy",
                           "Media Literacy","Critical Thinking",
                           "Creative Thinking","Transfer")
        corpus <- clusters %>%
                VectorSource %>%
                Corpus %>%
                CorpusClean
        # for (i in 1:length(corpus)) {
        #         meta(corpus[[i]], tag = "cluster") <- cluster.names[[i]]
        # }
}

#Get Corpus and Clean (stem & complete) from Atls Skill Cluster DF
GetCorpusFromClusters_mod <- function(clusters) {
        cluster.names <- c("Communication","Collaboration","Organization",
                           "Affective","Reflection","Information Literacy",
                           "Media Literacy","Critical Thinking",
                           "Creative Thinking","Transfer")
        corpus <- clusters %>%
                VectorSource %>%
                Corpus %>%
                CorpusClean_mod
        # for (i in 1:length(corpus)) {
        #         meta(corpus[[i]], tag = "cluster") <- cluster.names[[i]]
        # }
}

topn <- function(dtm, num) {
        mat <- dtm %>% as.matrix
        
        cluster.topn <- vector(mode = "list", length = ncol(mat))
        
        names(cluster.topn) <- cluster.names  
        for (i in 1:ncol(mat)) {
                top10 <- mat[,i] %>% 
                        sort(decreasing = TRUE) %>%
                        head(n =num)
                cluster.topn[[i]] <- top10
        }
        cluster.topn
}

#weighting functions
tfidf <- function(x) {
        weightTfIdf(x, normalize = TRUE)
}

#gets totals for dict words from corpus
GetDictTotalsfromCorpus <- function(corpus, dict) {
        ref <- DocumentTermMatrix(corpus, list(dictionary = dict))
        sums <- ref %>% as.matrix %>% colSums
        sums
}

skill.clusters <- c(
"Communication skills
How can students communicate through interaction?
Exchanging thoughts, messages and information effectively through interaction
• Give and receive meaningful feedback
• Use intercultural understanding to interpret communication
• Use a variety of speaking techniques to communicate with a variety of audiences
• Use appropriate forms of writing for different purposes and audiences
• Use a variety of media to communicate with a range of audiences
• Interpret and use effectively modes of non-verbal communication
• Negotiate ideas and knowledge with peers and teachers
• Participate in, and contribute to, digital social media networks
• Collaborate with peers and experts using a variety of digital environments and media
• Share ideas with multiple audiences using a variety of digital environments and media
How can students demonstrate communication through language?
Reading, writing and using language to gather and communicate information
• Read critically and for comprehension
• Read a variety of sources for information and for pleasure
• Make inferences and draw conclusions
• Use and interpret a range of discipline-specific terms and symbols
• Write for different purposes
• Understand and use mathematical notation
• Paraphrase accurately and concisely
• Preview and skim texts to build understanding
• Take effective notes in class
• Make effective summary notes for studying
• Use a variety of organizers for academic writing tasks
• Find information for disciplinary and interdisciplinary inquiries, using a variety of media
• Organize and depict information logically
• Structure information in summaries, essays and reports"
,
"Collaboration skills
How can students collaborate?
Working effectively with others
• Use social media networks appropriately to build and develop relationships
• Practise empathy
• Delegate and share responsibility for decision-making
• Help others to succeed
• Take responsibility for one’s own actions
• Manage and resolve conflict, and work collaboratively in teams
• Build consensus
• Make fair and equitable decisions
• Listen actively to other perspectives and ideas
• Negotiate effectively
• Encourage others to contribute
• Exercise leadership and take on a variety of roles within groups
• Give and receive meaningful feedback
• Advocate for one’s own rights and needs"
,
"Organization skills
How can students demonstrate organization skills?
Managing time and tasks effectively
• Plan short- and long-term assignments; meet deadlines
• Create plans to prepare for summative assessments (examinations and performances)
• Keep and use a weekly planner for assignments
• Set goals that are challenging and realistic
• Plan strategies and take action to achieve personal and academic goals
• Bring necessary equipment and supplies to class
• Keep an organized and logical system of information files/notebooks
• Use appropriate strategies for organizing complex information
• Understand and use sensory learning preferences (learning styles)
• Select and use technology effectively and productively"
,
"Affective skills
How can students manage their own state of mind?
Managing state of mind
• Mindfulness
– Practise focus and concentration
– Practise strategies to develop mental focus
– Practise strategies to overcome distractions
– Practise being aware of body–mind connections
• Perseverance
– Demonstrate persistence and perseverance
– Practise delaying gratification
• Emotional management
– Practise strategies to overcome impulsiveness and anger
– Practise strategies to prevent and eliminate bullying
– Practise strategies to reduce stress and anxiety
• Self-motivation
– Practise analysing and attributing causes for failure
– Practise managing self-talk
– Practise positive thinking
• Resilience
– Practise “bouncing back” after adversity, mistakes and failures
– Practise “failing well”
– Practise dealing with disappointment and unmet expectations
– Practise dealing with change"
,
"Reflection skills
How can students be reflective?
(Re)considering the process of learning; choosing and using ATL skills
• Develop new skills, techniques and strategies for effective learning
• Identify strengths and weaknesses of personal learning strategies (self-assessment)
• Demonstrate flexibility in the selection and use of learning strategies
• Try new ATL skills and evaluate their effectiveness
• Consider content
– What did I learn about today?
– What don’t I yet understand?
– What questions do I have now?
• Consider ATL skills development
– What can I already do?
– How can I share my skills to help peers who need more practice?
– What will I work on next?
• Consider personal learning strategies
– What can I do to become a more efficient and effective learner?
– How can I become more flexible in my choice of learning strategies?
– What factors are important for helping me learn well?
• Focus on the process of creating by imitating the work of others
• Consider ethical, cultural and environmental implications
• Keep a journal to record reflections"
,
"Information literacy skills
How can students demonstrate information literacy?
Finding, interpreting, judging and creating information
• Collect, record and verify data
• Access information to be informed and inform others
• Make connections between various sources of information
• Understand the benefits and limitations of personal sensory learning preferences when accessing, processing and recalling information
• Use memory techniques to develop long-term memory
• Present information in a variety of formats and platforms
• Collect and analyse data to identify solutions and make informed decisions
• Process data and report results
• Evaluate and select information sources and digital tools based on their appropriateness to specific tasks
• Understand and use technology systems
• Use critical-literacy skills to analyse and interpret media communications
• Understand and implement intellectual property rights
• Create references and citations, use footnotes/endnotes and construct a bibliography according to recognized conventions
• Identify primary and secondary sources"
,
"Media literacy skills
How can students demonstrate media literacy?
Interacting with media to use and create ideas and information
• Locate, organize, analyse, evaluate, synthesize and ethically use information from a variety of sources and media (including digital social media and online networks)
• Demonstrate awareness of media interpretations of events and ideas (including digital social media)
• Make informed choices about personal viewing experiences
• Understand the impact of media representations and modes of presentation
• Seek a range of perspectives from multiple and varied sources
• Communicate information and ideas effectively to multiple audiences using a variety of media and formats
• Compare, contrast and draw connections among (multi)media resources"
,	
"Critical-thinking skills
How can students think critically?
Analysing and evaluating issues and ideas
• Practise observing carefully in order to recognize problems
• Gather and organize relevant information to formulate an argument
• Recognize unstated assumptions and bias
• Interpret data
• Evaluate evidence and arguments
• Recognize and evaluate propositions
• Draw reasonable conclusions and generalizations
• Test generalizations and conclusions
• Revise understanding based on new information and evidence
• Evaluate and manage risk
• Formulate factual, topical, conceptual and debatable questions
• Consider ideas from multiple perspectives
• Develop contrary or opposing arguments
• Analyse complex concepts and projects into their constituent parts and synthesize them to create new understanding
• Propose and evaluate a variety of solutions
• Identify obstacles and challenges
• Use models and simulations to explore complex systems and issues
• Identify trends and forecast possibilities
• Troubleshoot systems and applications"
,
"Creative Thinking Skills
How can students be creative?
Generating novel ideas and considering new perspectives
• Use brainstorming and visual diagrams to generate new ideas and inquiries
• Consider multiple alternatives, including those that might be unlikely or impossible
• Create novel solutions to authentic problems
• Make unexpected or unusual connections between objects and/or ideas
• Design improvements to existing machines, media and technologies
• Design new machines, media and technologies
• Make guesses, ask “what if” questions and generate testable hypotheses
• Apply existing knowledge to generate new ideas, products or processes
• Create original works and ideas; use existing works and ideas in new ways
• Practise flexible thinking—develop multiple opposing, contradictory and complementary arguments
• Practise visible thinking strategies and techniques
• Generate metaphors and analogies"
,
"Transfer Skills
How can students transfer skills and knowledge across disciplines and subject groups?
Using skills and knowledge in multiple contexts
• Use effective learning strategies in subject groups and disciplines
• Apply skills and knowledge in unfamiliar situations
• Inquire in different contexts to gain a different perspective
• Compare conceptual understanding across multiple subject groups and disciplines
• Make connections between subject groups and disciplines
• Combine knowledge, understanding and skills to create products or solutions
• Transfer current knowledge to learning of new technologies
• Change the context of an inquiry to gain different perspectives"
)


cluster.names <- c("Communication","Collaboration","Organization","Affective","Reflection","Information Literacy","Media Literacy","Critical Thinking","Creative Thinking","Transfer")
cluster.names.stemmed <- c("communic", "collabor", "organ", "affect", "reflect", "inform", 
                           "media", "critic", "creativ", "transfer")
atls.vector <- setNames(skill.clusters, cluster.names)
