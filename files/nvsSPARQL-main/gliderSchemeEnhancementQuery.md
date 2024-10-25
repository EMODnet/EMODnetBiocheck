The URL <p>
https://vocabtst.nerc.ac.uk/sparql/sparql?query=PREFIX%20skos%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2004%2F02%2Fskos%2Fcore%23%3E%0APREFIX%20rdf%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F1999%2F02%2F22-rdf-syntax-ns%23%3E%0APREFIX%20rdfs%3A%20%3Chttp%3A%2F%2Fwww.w3.org%2F2000%2F01%2Frdf-schema%23%3E%0A%0Aselect%20*%20where%20%7B%20%0A%3FURL%20skos%3AinScheme%20%20%20%20%3Chttp%3A%2F%2Fvocabtst.nerc.ac.uk%2Fscheme%2FGLIDER_SENSORS%2Fcurrent%2F%3E%20.%0A%20%20%20%3FURL%20skos%3AprefLabel%20%3FprefLabel%20.%0A%3FURL%20skos%3AaltLabel%20%3Falt%20.%0A%0A%3FURL%20skos%3Adefinition%20%3Fdef%20.%0A%0A%7D%20&format=json

   <b>To run the query: <p></b>
   Go to https://vocab.nerc.ac.uk/sparql/ and copy past the following query:<p>
      
   PREFIX skos: <http://www.w3.org/2004/02/skos/core#>   
   PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>   
   PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>  

   select * where {  
   ?URL skos:inScheme    <http://vocab.nerc.ac.uk/scheme/GLIDER_SENSORS/current/> . 
   ?URL skos:prefLabel ?prefLabel .    
   ?URL skos:altLabel ?alt .  
   ?URL skos:definition ?def .   
   } 
