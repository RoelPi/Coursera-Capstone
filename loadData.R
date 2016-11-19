if (!file.exists('Coursera-Swiftkey.zip')) {
    download.file('https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip','Coursera-Swiftkey.zip',quiet=F)
}
if (!file.exists()) {
    unzip('Coursera-Swiftkey.zip',exdir='dataset')
}
