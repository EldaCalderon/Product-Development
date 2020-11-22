create database parcial1;
use parcial1;


#Creacion de tablas
create table tabla1(
    videoID varchar(15) primary key,
    viewCount int,
    likeCount int,
    dislikeCount int,
    favoriteCount int,
    commentCount int
);

create table tabla2(
    kind varchar(25),
    etag varchar(30),
    id varchar(50),
    contentVideoID varchar(15) primary key,
    details varchar(25)
);

create table tabla3(
    videoID varchar(15) primary key,
    title varchar(100),
    description varchar(200),
    iframe varchar(200),
    link varchar(200)
);


