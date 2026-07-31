package vecxt.audit

import java.io.File

/** The audit, run once for every suite in this module, and its report written as a side effect of the first access.
  *
  * Writing the report here rather than from a check means it is produced whether the checks pass or fail — which is the
  * case that matters, since a failing run is exactly when somebody needs to read it.
  */
object Audited:

  val config: Config = Config.fromSystemProperties()

  lazy val result: AuditResult =
    val r = Audit.run(config)
    Report.write(r, config.reportDir)
    r
  end result

  /** Where `CanarySuite` finds its fixtures: compiled separately and on no audited root. */
  object canary:
    def classes: os.Path = os.Path(prop("vecxt.audit.canaryClasses"))
    def sources: Seq[os.Path] =
      prop("vecxt.audit.canarySources").split(File.pathSeparatorChar).toSeq.filter(_.nonEmpty).map(os.Path(_))
  end canary

  private def prop(name: String): String =
    sys.props.getOrElse(name, sys.error(s"system property '$name' is not set — see bytecodeAudit/package.mill"))

end Audited
