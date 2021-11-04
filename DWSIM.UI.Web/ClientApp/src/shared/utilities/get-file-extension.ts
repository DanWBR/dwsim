export const getFileExtension = (filename: string) => {
    var filenameArray = filename.split('.');
    if (filenameArray && filenameArray.length > 1) {
        return filenameArray.pop();
    } else {
        return undefined;
    }
}