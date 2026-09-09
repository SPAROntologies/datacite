# Competency Questions

The DataCite Ontology can be used for answering several questions related to related to the identification, attribution, and discoverability of research products within a scholarly knowledge graph. 
In the following subsections, some of them are introduced together with their respective SPARQL queries. 

The prefixes that are used in all the SPARQL queries provided below are defined as follows:

    PREFIX datacite: <http://purl.org/spar/datacite/>
    PREFIX dcterms: <http://purl.org/dc/terms/>
    PREFIX fabio: <http://purl.org/spar/fabio/>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX literal: <http://www.essepuntato.it/2010/06/literalreification/>

## CQ1

Which article provides the description for a specific dataset identified by the DOI \"10.5061/dryad.mq8r2\"?

    SELECT ?article ?article_doi
    WHERE {
        ?dataset_id literal:hasLiteralValue \"10.5061/dryad.mq8r2\" .
        ?dataset datacite:hasIdentifier ?dataset_id ;
            a fabio:Dataset ;
            datacite:hasDescription ?article .
        
        ?article datacite:hasIdentifier ?paper_id .
        ?paper_id literal:hasLiteralValue ?article_doi .
    }

## CQ2

Who are the creators associated with both the dataset and the article?

    SELECT DISTINCT ?name ?orcid_uri
    WHERE {
        ?dataset a fabio:Dataset ;
                dcterms:creator ?orcid_uri .
        
        ?article a fabio:JournalArticle ;
                dcterms:creator ?orcid_uri .
                
        ?orcid_uri foaf:name ?name .
    }

## CQ3

What are the names and ORCID iDs of all researchers mentioned in the graph?

    SELECT ?name ?orcid_value
    WHERE {
        ?person a foaf:Person ;
                foaf:name ?name ;
                datacite:hasIdentifier ?id_node .
        
        ?id_node datacite:usesIdentifierScheme datacite:orcid ;
                literal:hasLiteralValue ?orcid_value .
    }

## CQ4

List all resources that have a description type categorized as \"other\".

    SELECT ?resource
    WHERE {
        ?resource datacite:hasDescriptionType datacite:other .
    }

## CQ5

How many identifiers are registered for each identifier scheme?

    SELECT ?scheme (COUNT(?id_node) AS ?count)
    WHERE {
        ?id_node datacite:usesIdentifierScheme ?scheme .
    }
    GROUP BY ?scheme