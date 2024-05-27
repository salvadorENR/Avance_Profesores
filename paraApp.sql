-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

CREATE DATABASE IF NOT EXISTS avance_profesores;

USE avance_profesores;

CREATE TABLE IF NOT EXISTS page_data (
    id INT AUTO_INCREMENT PRIMARY KEY,
    Department VARCHAR(255),
    Grade VARCHAR(10),
    Page INT
);
