MATCH
	(guava:ARTIFACT),
	(commons_io:ARTIFACT),
	(jre:ARTIFACT)
WHERE
	commons_io.groupId = 'commons-io'
	AND commons_io.artifactId = 'commons-io'
	AND commons_io.version = '2.6'
	AND guava.groupId = 'com.google.guava'
	AND guava.artifactId = 'guava'
	AND guava.version = '23.5-jre'
	AND jre.artifactId = 'jre'
WITH
	guava,
	commons_io,
	jre
MATCH
	(a:ARTIFACT)-[:DEPENDS_ON]->(b:ARTIFACT)
WITH
	guava,
	commons_io,
	jre,
	a,
	collect(b) AS bs
RETURN
	a.groupId AS groupId,
	a.artifactId AS artifactId,
	a.version AS version,
	(guava IN bs) AS guava,
	(commons_io IN bs) AS commons_io,
	(jre IN bs) AS jre
ORDER BY
	groupId,
	artifactId,
	version
