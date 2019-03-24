---
layout: post
published: true
title: Using the GitHub API in Pharo 7
header-img: img/posts/ForksAndStars.jpg
subtitle: >-
  In my mining activities with Pharo, I found it useful to query GitHub to
  select projects to mine.
---

![Forks and Stars on top of a table]({{site.baseurl}}/img/posts/ForksAndStars.jpg){:class="img-responsive"} In my mining activities with Pharo, I found it useful to query GitHub to select projects to mine according to their popularity (e.g., forks and stars).

## Which API (REST or GraphQL)?

GitHub offers different ways to query its infrastructure. The traditional REST query is the v3 of its API, and a more powerful (which requires an authentication token) GraphQL v4 of its API.

## REST (v3 API) 

GitHub's v3 API for REST is OK for getting info about projects, such as the ones that have lots of stars or forks. 

Here's a snippet of code to show how to use it in Pharo:
```
znClient := ZnClient new.
jsonString := znClient
    get:
        'https://api.github.com/search/repositories?q=language:java+stars:>=500+forks:>100&sort=stars&order=desc&per_page=5'.
```

>Note that we have some specifics in the query, e.g. `language:java` and `sort=stars`. You can read more about these options on [GitHub's documentation for API v3](https://developer.github.com/v3/). 

>Note that the query I defined returns at most 5 results per query (`per_page=5`). If you don't specify this option, GitHub gives 30 results per query. It's possible to get up to 100 results per query by setting the value. To get mutliple "pages", you specify the page number (`page=2`, `page=3`, etc.) in queries.

The `jsonString` is a JSON string response, which can be accessed nicely using [NeoJSON](https://github.com/svenvc/NeoJSON) (which must be loaded in your Pharo image).

### NeoJSON to get to the values 

Assuming you loaded NeoJSON, here's how you could get to the values from the `jsonString` response above:

```smalltalk
queryDic := NeoJSONReader fromString: jsonString.
projectsColl := (queryDic at: 'items')
    collect: [ :item | 
        {(item at: 'full_name').
        (item at: 'git_url').
        (item at: 'watchers_count').
        (item at: 'forks_count')} ].
    
projectsColl asString        
"'#(#(''iluwatar/java-design-patterns'' ''git://github.com/iluwatar/java-design-patterns.git'' 
      45632 14784)
    #(''elastic/elasticsearch'' ''git://github.com/elastic/elasticsearch.git'' 
      39349 13141) 
    #(''ReactiveX/RxJava'' ''git://github.com/ReactiveX/RxJava.git'' 
      38107 6443) 
    #(''spring-projects/spring-boot'' ''git://github.com/spring-projects/spring-boot.git'' 
      35550 23522) 
    #(''kdn251/interviews'' ''git://github.com/kdn251/interviews.git'' 
      33272 6610))'"       
```

Finally, I was hoping to get data about how many issues (total) existed for projects, and it proved troublesome with this limited version of the API. It's possible to get, but requires multiple calls, which amounts to clicking through each repo's issues REST page. Googling the problem, I found the mention of GitHub encouraging people to move to GraphQL.

## GraphQL (v4 API)

GraphQL is more complex, but in theory more powerful. You have to do POST requests, and you need an authorization token (which means GitHub tracks what you are doing). Since the responses are still in JSON, NeoJSON can pick them apart for you as before.

So, here's what I did according to the instructions from GitHub [here](https://developer.github.com/v4/guides/forming-calls/):

- I first [created a token](https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line) with scope `repo` (and all repo sub-scopes). I described it as a GitHub repo mining token. For obvious reasons, I don't reveal the token here in my examples.
- Before making the first query call, you have to [make a call to login with the token](https://developer.github.com/v4/guides/forming-calls/#communicating-with-graphql). 
- Subsequent calls specify the token as before.

Here's a Playground script that could be converted easily to a class with methods:

```smalltalk
| graphQLLoginString graphQLGetIssueCount doGraphQLQuery myAPIToken response queryDic issueCount |
graphQLLoginString := 
    'query { viewer { login }}'.
graphQLGetIssueCount := 
    'query { repository(owner:"isaacs", name:"github") {issues(states:OPEN) {totalCount}}}'.
"Get a token for repo scope 
 at https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line"
myAPIToken := 'GET_YOUR_TOKEN_FROM_GIT_HUB_AND_PUT_IT_HERE'.
"block closure to be called like a method"
doGraphQLQuery := [ :authToken :graphQLquery | 
    | escaper |
    "escape any double-quotes in the GraphQL query"
    escaper := [ :stringToEscape | 
        stringToEscape copyWithRegex: '\"' matchesReplacedWith: '\"' ].
    ZnClient new
        url: 'https://api.github.com/graphql';
        headerAt: 'Authorization' put: 'bearer ' , authToken;
        entity:
            (ZnEntity
                with: '{"query": "' , (escaper value: graphQLquery) , '"}'
                type: ZnMimeType applicationJson);
        post ].
"Log in first, should check for errors in the reponse..."
response := doGraphQLQuery 
    value: myAPIToken value: graphQLLoginString.
"Do the query to get the number of issues"
response := doGraphQLQuery
    value: myAPIToken
    value: graphQLGetIssueCount.
"Pull the number out of the JSON"
queryDic := NeoJSONReader fromString: response.
issueCount := ((((queryDic at: 'data') at: 'repository') at: 'issues')
    at: 'totalCount') asInteger
```

[Photo credit](https://www.maxpixel.net/Metal-Metal-Fork-Spoon-Fork-Cutlery-Close-2390507)
