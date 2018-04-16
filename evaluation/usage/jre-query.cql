MATCH
	(a:ARTIFACT)-[:CONTAINS]->(c:CLASS)-[:DECLARES]->(m:METHOD)<-[x:INVOKES]-(:METHOD)<-[:DECLARES]-(:CLASS)<-[:CONTAINS]-(b:ARTIFACT)
WHERE
	a.artifactId = 'jre'
	AND size(x.parameters) > 0
UNWIND range(0, size(x.parameters) - 1) AS i
RETURN
	c.name AS class,
	'method' AS attribute,
	m.visibility AS visibility,
	m.bytecodeSignature AS name,
	exists((m)-[:GETS]->(:FIELD)) AS getter,
	i AS index,
	a = b AS internal,
	x.parameters[i] AS nullness,
	count(*) AS n
UNION ALL
MATCH
	(a:ARTIFACT)-[:CONTAINS]->(c:CLASS)-[:DECLARES]->(m:METHOD)<-[p:POINTS_TO]-(d:DEREFERENCE)<-[:DEREFERENCES]-(:METHOD)<-[:DECLARES]-(:CLASS)<-[:CONTAINS]-(b:ARTIFACT)
WHERE
	a.artifactId = 'jre'
RETURN
	c.name AS class,
	'method' AS attribute,
	m.visibility AS visibility,
	m.bytecodeSignature AS name,
	exists((m)-[:GETS]->(:FIELD)) AS getter,
	-1 AS index,
	a = b AS internal,
	p.nullness AS nullness,
	count(*) AS n
