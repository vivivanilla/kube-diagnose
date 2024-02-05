package com.github.vivivanilla.kubediagnose

import scala.jdk.CollectionConverters.*

extension[A] (list: java.util.List[A]) def convertToScala: Seq[A] =
    Option(list).map(_.asScala).toSeq.flatten
