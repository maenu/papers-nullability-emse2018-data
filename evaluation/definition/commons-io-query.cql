MATCH
	(a:ARTIFACT)-[:CONTAINS]->(c:CLASS)-[:DECLARES]->(m:METHOD)
WHERE
	a.groupId = 'commons-io'
	AND a.artifactId = 'commons-io'
	AND a.version = '2.6'
	AND size(m.parameters) > 0
UNWIND range(0, size(m.parameters) - 1) AS i
RETURN
	c.name AS class,
	'method' AS attribute,
	m.visibility AS visibility,
	m.bytecodeSignature AS name,
	i AS index,
	m.parametersPrimitive[i] AS primitive,
	m.parameters[i] AS nullness
UNION ALL
MATCH
	(a:ARTIFACT)-[:CONTAINS]->(c:CLASS)-[:DECLARES]->(m:METHOD)
WHERE
	a.groupId = 'commons-io'
	AND a.artifactId = 'commons-io'
	AND a.version = '2.6'
RETURN
	c.name AS class,
	'method' AS attribute,
	m.visibility AS visibility,
	m.bytecodeSignature AS name,
	-1 AS index,
	m.primitive AS primitive,
	m.returns AS nullness
ORDER BY
	class,
	attribute,
	visibility,
	name,
	index
