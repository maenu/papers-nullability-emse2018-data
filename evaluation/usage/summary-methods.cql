MATCH
	(a:ARTIFACT)-[:CONTAINS]->(:CLASS)-[:DECLARES]->(m:METHOD)<-[x:INVOKES]-(:METHOD)<-[:DECLARES]-(:CLASS)<-[:CONTAINS]-(b:ARTIFACT)
RETURN
	a.artifactId AS artifactId,
	'invocation' as type,
	a = b AS internal,
	count(DISTINCT m) AS methods,
	count(x) AS n
UNION ALL
MATCH
	(a:ARTIFACT)-[:CONTAINS]->(:CLASS)-[:DECLARES]->(m:METHOD)<-[:POINTS_TO]-(d:DEREFERENCE)<-[:DEREFERENCES]-(:METHOD)<-[:DECLARES]-(:CLASS)<-[:CONTAINS]-(b:ARTIFACT)
RETURN
	a.artifactId AS artifactId,
	'dereference' as type,
	a = b AS internal,
	count(DISTINCT m) AS methods,
	count(d) AS n
