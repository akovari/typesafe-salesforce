package com.github.akovari.typesafeSalesforce;

import org.apache.commons.collections.map.MultiValueMap;
import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.annotation.processing.SupportedSourceVersion;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.*;
import javax.tools.Diagnostic;
import javax.tools.FileObject;
import javax.tools.StandardLocation;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlType;
import java.io.IOException;
import java.io.Writer;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

/**
 * User: akovari
 * Date: 11/2/13
 * Time: 6:53 PM
 */
@SupportedAnnotationTypes({"com.github.akovari.typesafeSalesforce.annotations.SalesForceEntity"})
@SupportedSourceVersion(SourceVersion.RELEASE_8)
public class SalesForceAnnotationProcessor extends AbstractProcessor {
    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        try {
            Properties props = new Properties();
            URL url = this.getClass().getClassLoader().getResource("velocity.properties");
            props.load(url.openStream());

            VelocityEngine ve = new VelocityEngine(props);
            ve.init();

            MultiValueMap allMetaModels = new MultiValueMap();

            for (TypeElement te : annotations) {
                for (Element e : roundEnv.getElementsAnnotatedWith(te)) {
                    try {
                        Map<String, VariableElement> consts = new HashMap<>();

                        TypeElement classElement = (TypeElement) e;
                        PackageElement packageElement = (PackageElement) classElement.getEnclosingElement();

                        XmlType xmlType = classElement.getAnnotation(XmlType.class);

                        e.getEnclosedElements().stream().filter(ee -> ElementKind.FIELD.equals(ee.getKind())).forEach(ee -> {
                            VariableElement varElement = (VariableElement) ee;

                            XmlElement xmlElement = varElement.getAnnotation(XmlElement.class);
                            XmlElementRef xmlElementRef = varElement.getAnnotation(XmlElementRef.class);

                            if (xmlElement != null && xmlElement.name() != null && !xmlElement.name().equals("##default")) {
                                consts.put(xmlElement.name(), varElement);
                            } else if (xmlElementRef != null && xmlElementRef.name() != null) {
                                consts.put(xmlElementRef.name(), varElement);
                            }
                        });

                        VelocityContext vc = new VelocityContext();

                        vc.put("className", classElement.getSimpleName().toString());
                        vc.put("packageName", packageElement.getQualifiedName().toString());
                        vc.put("consts", consts);
                        vc.put("entityName", xmlType.name());

                        allMetaModels.put(packageElement, classElement);

                        Template vt = ve.getTemplate("sfMetaModelClass.vm");

                        FileObject jfo = processingEnv.getFiler().createResource(StandardLocation.SOURCE_OUTPUT, packageElement.getQualifiedName().toString(), classElement.getQualifiedName().toString() + "_.scala");

                        processingEnv.getMessager().printMessage(Diagnostic.Kind.NOTE, "creating source file: " + jfo.toUri());

                        Writer writer = jfo.openWriter();

                        processingEnv.getMessager().printMessage(Diagnostic.Kind.NOTE, "applying velocity template: " + vt.getName());

                        vt.merge(vc, writer);

                        writer.close();
                    } catch (IOException ex) {
                        ex.printStackTrace();
                    }
                }
            }
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        return true;
    }
}
