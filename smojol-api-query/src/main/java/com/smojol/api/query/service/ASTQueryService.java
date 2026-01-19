package com.smojol.api.query.service;

import com.smojol.api.query.model.CBLFile;
import com.smojol.api.query.model.Copybook;
import com.smojol.api.query.model.Dataset;
import com.smojol.api.query.model.JCLFile;

import java.util.List;
import java.util.Optional;

/**
 * Service pour queryier et naviguer dans les ASTs COBOL
 * Expose 12 méthodes pour accéder aux données et relationships
 */
public interface ASTQueryService {

    /**
     * Récupère un programme COBOL par son nom
     *
     * @param programName nom du programme (ex: "CBIMPORT")
     * @return Optional contenant le CBLFile si trouvé
     */
    Optional<CBLFile> getCbl(String programName);

    /**
     * Récupère un fichier JCL par son nom
     *
     * @param jclName nom du fichier JCL (ex: "CBIMPORT")
     * @return Optional contenant le JCLFile si trouvé
     */
    Optional<JCLFile> getJcl(String jclName);

    /**
     * Récupère un copybook par son nom
     *
     * @param copybookName nom du copybook (ex: "CVACT01Y")
     * @return Optional contenant le Copybook si trouvé
     */
    Optional<Copybook> getCopybook(String copybookName);

    /**
     * Récupère un dataset par son nom
     *
     * @param datasetName nom du dataset (ex: "CUSTOUT")
     * @return Optional contenant le Dataset si trouvé
     */
    Optional<Dataset> getDataset(String datasetName);

    /**
     * Trouve tous les fichiers JCL qui utilisent un programme COBOL
     *
     * @param programName nom du programme COBOL (ex: "CBIMPORT")
     * @return List des JCLFiles qui utilisent ce programme
     */
    List<JCLFile> findJclUsingCbl(String programName);

    /**
     * Trouve tous les fichiers JCL qui accèdent un dataset
     *
     * @param datasetName nom du dataset (ex: "CUSTOUT")
     * @return List des JCLFiles qui accèdent ce dataset
     */
    List<JCLFile> findJclUsingDataset(String datasetName);

    /**
     * Trouve tous les programmes COBOL qui utilisent un copybook
     *
     * @param copybookName nom du copybook (ex: "CVACT01Y")
     * @return List des CBLFiles qui utilisent ce copybook
     */
    List<CBLFile> findCblUsingCopybook(String copybookName);

    /**
     * Trouve tous les programmes COBOL qui accèdent un dataset
     *
     * @param datasetName nom du dataset (ex: "CUSTOUT")
     * @return List des CBLFiles qui accèdent ce dataset
     */
    List<CBLFile> findCblUsingDataset(String datasetName);

    /**
     * Trouve tous les programmes appelés par un programme donné (direct calls)
     *
     * @param programName nom du programme (ex: "CBIMPORT")
     * @return List des CBLFiles appelés par ce programme
     */
    List<CBLFile> findCblCallees(String programName);

    /**
     * Trouve tous les programmes qui appellent un programme donné (reverse calls)
     *
     * @param programName nom du programme (ex: "CBIMPORT")
     * @return List des CBLFiles qui appellent ce programme
     */
    List<CBLFile> findCblCallers(String programName);

    /**
     * Trouve tous les copybooks utilisés par un programme COBOL
     *
     * @param programName nom du programme (ex: "CBIMPORT")
     * @return List des Copybooks utilisés par ce programme
     */
    List<Copybook> findCopybooksUsedByCbl(String programName);

    /**
     * Trouve tous les copybooks utilisés par un copybook (includes)
     * Détecte et évite les dépendances circulaires
     *
     * @param copybookName nom du copybook (ex: "CVACT01Y")
     * @return List des Copybooks inclus par ce copybook (sans cycles)
     */
    List<Copybook> findCopybooksUsedByCopybook(String copybookName);

    /**
     * Récupère tous les programmes COBOL disponibles
     *
     * @return List de tous les CBLFiles
     */
    List<CBLFile> getAllCbl();

    /**
     * Récupère tous les fichiers JCL disponibles
     *
     * @return List de tous les JCLFiles
     */
    List<JCLFile> getAllJcl();

    /**
     * Récupère tous les copybooks disponibles
     *
     * @return List de tous les Copybooks
     */
    List<Copybook> getAllCopybooks();

    /**
     * Récupère tous les datasets disponibles
     *
     * @return List de tous les Datasets
     */
    List<Dataset> getAllDatasets();
}
