# GithubRecommender
GithubRecommender

# General plan

## Initial data mining (build ontology)
1. scan githubarchive for interesting events (updates, creates, ...)
1.2 dedup data
2. fetch repository data from github api (enrichment)
3. build topic ontology

## Data mining with existing ontology
Needs to be done repeatedly (reindex process)

1. extract topics from descriptions

## Recommendation based on topics
1. trigger (user wants reco) => user-id
   Idea: when user stars project, we get the event and can trigger the recommendation
   
2. fetch user-id => [repositories, starred repositorys]
   if the repos are not yet known, execute data mining for it
   
3. build bagofwords model from topics (of their repos)
   (need to decide whether we use sets or multisets)
   
4. compute similarity for topics

## Recommendation based on languages
Not yet planned



