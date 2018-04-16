// dereferences within guava

MATCH
	(a:ARTIFACT)-[:CONTAINS]->(c:CLASS)-[:IMPLEMENTS]->(m:METHOD)
WHERE
	a.groupId = 'com.google.guava'
	AND a.artifactId = 'guava'
	AND a.version = '23.5-jre'
WITH
	a,
	c,
	m,
	rand() AS r
ORDER BY r
LIMIT 100
OPTIONAL MATCH (m)-[i:INVOKES]->(dm:METHOD)<-[:DECLARES]-(dc:CLASS)<-[:CONTAINS]-(a)
RETURN
	c.name AS class,
	m.bytecodeSignature AS method,
	i IS NOT NULL AS invokes,
	i.parameters AS parameters,
	i.sourceLocation AS location,
	dc.name AS originClass,
	dm.bytecodeSignature AS originMethod
