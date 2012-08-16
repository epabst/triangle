package com.github.triangle

/**
 * The input to a [[com.github.triangle.PortableField]] transformer.
 * Usage in transformer: case TransformerInput(subject, valueOpt, ...see usage of GetterInput...) =>
 * @param subject the primary subject that will be transformed
 * @param valueOpt the value to put into the subject
 * @param context any context items (which may or may not include the primary subject) that may be useful.
 * @author Eric Pabst (epabst@gmail.com)
 *         Date: 8/16/12
 *         Time: 8:41 AM
 */
case class TransformerInput[S <: AnyRef,T](subject: S, valueOpt: Option[T], context: GetterInput[AnyRef])