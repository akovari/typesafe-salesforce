package com.github.akovari.typesafeSalesforce.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * User: akovari
 * Date: 11/3/13
 * Time: 1:12 PM
 */
@Target({ElementType.TYPE})
@Retention(RetentionPolicy.SOURCE)
public @interface SalesForceEntity {
}
