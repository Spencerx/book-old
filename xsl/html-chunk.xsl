<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml" version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    
    <xsl:import href="http://docbook.sourceforge.net/release/xsl/current/xhtml/chunk.xsl" />

    <xsl:param name="nlpwpWebsite" select="0"/>
    
    <xsl:output method="xml"
        encoding="UTF-8"
        indent="yes"/>
    
    <!-- Chunking -->
    <xsl:param name="chunker.output.indent" select="'no'" />
    <xsl:param name="use.id.as.filename" select="1" />
    <xsl:param name="chunk.section.depth" select="0" />
    <xsl:param name="chunker.output.encoding">UTF-8</xsl:param>
    
    <!-- Default CSS stylesheet -->
    <xsl:param name="html.stylesheet">screen.css</xsl:param>
    
    <xsl:param name="html.ext">.xhtml</xsl:param>
    
    <xsl:param name="toc.section.depth" select="1" />
    
    <xsl:param name="section.autolabel" select="1" />
    <xsl:param name="section.label.includes.component.label" select="1" />

    <!-- No 'title' atttributes. -->
    <xsl:template match="*" mode="html.title.attribute"/>
    
    <!-- Add 'head' elements. -->
    <xsl:template name="user.head.content">
        <xsl:if test="$nlpwpWebsite = 1">
            <script type="text/javascript" src="/MathJax/MathJax.js?config=default"/>
        </xsl:if>
    </xsl:template>
    
    <xsl:template name="user.footer.navigation">
        <xsl:if test="$nlpwpWebsite = 1">
          <script type="text/javascript">
  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-23887964-1']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();
          </script>
        </xsl:if>
    </xsl:template>
</xsl:stylesheet>
